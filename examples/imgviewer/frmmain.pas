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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit frmmain;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Controls, Forms, LazFileUtils, LazUTF8,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ActnList, Menus, LCLType,
  fpreadtiff {adds TIFF format read support to TImage};

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    AOpen: TAction;
    AOpenDir: TAction;
    AExit: TAction;
    LBFiles: TListBox;
    SPImage: TSplitter;
    File1: TMenuItem;
    MIOpen: TMenuItem;
    MIOPenDir: TMenuItem;
    N1: TMenuItem;
    MIQuit: TMenuItem;
    TBOPen: TToolButton;
    TBOpenDir: TToolButton;
    ILMain: TImageList;
    ODImage: TOpenDialog;
    AClear: TAction;
    MIOpenDirRec: TMenuItem;
    MIClear: TMenuItem;
    OpenDirRecursively: TAction;
    TBOpenDirRec: TToolButton;
    ADoubleSize: TAction;
    MImage: TMenuItem;
    D1: TMenuItem;
    AHalfSize: TAction;
    MIHalfSize: TMenuItem;
    PImage: TPanel;
    ScrollBox1: TScrollBox;
    IMain: TImage;
    ANextImage: TAction;
    APreviousImage: TAction;
    ANextImageDir: TAction;
    APrevImageDir: TAction;
    MINextImage: TMenuItem;
    PreviousImage1: TMenuItem;
    Nextimagedirectory1: TMenuItem;
    Previousimagedirectory1: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton4: TToolButton;
    TBPRev: TToolButton;
    TBNext: TToolButton;
    TBPRevDir: TToolButton;
    TBNextDir: TToolButton;
    TBDoubleSize: TToolButton;
    TBHalfSize: TToolButton;
    ToolButton3: TToolButton;
    N2: TMenuItem;
    OpenDialog1: TOpenDialog;
    procedure AOpenExecute(Sender: TObject);
    procedure LBFilesClick(Sender: TObject);
    procedure AOpenDirExecute(Sender: TObject);
    procedure AExitExecute(Sender: TObject);
    procedure OpenDirRecursivelyExecute(Sender: TObject);
    procedure AClearExecute(Sender: TObject);
    procedure ADoubleSizeExecute(Sender: TObject);
    procedure AHalfSizeExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ANextImageExecute(Sender: TObject);
    procedure APreviousImageExecute(Sender: TObject);
    procedure ANextImageDirExecute(Sender: TObject);
    procedure APrevImageDirExecute(Sender: TObject);
  private
    FImageScale: double;
    procedure AddFile(FileName: string; ShowFile: boolean);
    procedure ShowFile(Index: integer);
    procedure AddDir(Directory: string; Recurse: boolean);
    procedure RescaleImage(NewScale: double);
    procedure NextImage;
    procedure PreviousImage;
    procedure NextImageDir;
    procedure PreviousImageDir;
    function NextDirIndex(Direction: integer): integer;
    procedure ShiftImageIndex(MoveBy: integer);
    procedure ProcessCommandLine;
    procedure DoError(Msg: string; Args: array of const);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  ImageTypes = '|.jpg|.jpeg|.bmp|.xpm|.png';

resourcestring
  SSelectImageDir = 'Select directory to add images from';
  SSelectImageDirRec = 'Select directory to recursively add images from';
  SImageViewer = 'Image viewer';
  SErrNeedArgument = 'Option at position%d (%s) needs an argument';

{ [] }
procedure TMainForm.AOpenExecute(Sender: TObject);

var
  I: integer;

begin
  with ODImage do
  begin
    if Execute then
      for I := 0 to Files.Count - 1 do
        AddFile(Files[I], (I = 0));
  end;
end;

procedure TMainForm.AddFile(FileName: string; ShowFile: boolean);
// Adds a file to the listbox and displays it if ShowFile is true
var
  Index: integer;

begin
  ShowFile := ShowFile or (LBFiles.Items.Count = 0);
  Index := LBFiles.Items.Add(FileName);
  if ShowFile then
    self.ShowFile(Index);
end;

procedure TMainForm.ShowFile(Index: integer);
// Loads file and displays it into the IMain TImage
var
  LoadOK: boolean;

begin
  if Index = -1 then
  begin
    IMain.Picture := nil;
    Caption := SImageViewer;
  end
  else
    repeat
      try
        LoadOK := false;
        IMain.Align := AlClient;
        Imain.Stretch := false;
        FImageScale := 1.0;
        IMain.Picture.LoadFromFile(LBFiles.Items[Index]);
        Caption := SImageViewer + ' (' + LBFiles.Items[Index] + ')';
        LoadOK := true;
      except
        // If we can't load the image, try next file unless we're at the end
        if Index < LBFiles.Items.Count - 1 then
          Inc(Index)
        else
          Index := -1;
      end
    until LoadOK or (Index = -1);

  // Now synchronize our listbox to the file we loaded:
  with LBFiles do
  begin
    if Index <> ItemIndex then
      LBFiles.ItemIndex := Index;
{    If Not ItemVisible(ItemIndex) then
      MakeCurrentVisible;}
  end;
end;

procedure TMainForm.LBFilesClick(Sender: TObject);
begin
  ShowFile(LBFiles.ItemIndex);
end;

procedure TMainForm.AOpenDirExecute(Sender: TObject);
// Open a single directory (non recursively)
var
  Dir: string;
  WasSorted: boolean;
begin
  if SelectDirectory(SSelectImageDir, '/', Dir, true) then
  begin
    Screen.BeginWaitCursor; //Show user he may have to wait for big directories
    try
      LBFiles.Items.BeginUpdate; //Indicate to the listbox that we're doing a lengthy operation
      WasSorted:=LBFiles.Sorted;
      LBFiles.Sorted:=true;
      AddDir(Dir, false);
      LBFiles.Sorted:=WasSorted;
    finally
      LBFiles.Items.EndUpdate;
      Screen.EndWaitCursor;
    end;
  end;
end;

procedure TMainForm.AddDir(Directory: string; Recurse: boolean);

var
  Info: TSearchRec;
  Ext: string;
begin
  Directory := IncludeTrailingPathDelimiter(Directory);
  if FindFirstUTF8(Directory + '*.*', 0, Info) = 0 then
    try
      repeat
        Ext := ExtractFileExt(Info.Name);
        // Support opening tiff files as well as the built-in image types.
        // Note: requires fpreadtiff in the uses clause to work.
        if Pos(lowercase('|'+Ext+'|'), ImageTypes+'|.tif|.tiff|') <> 0 then
          AddFile(Directory + Info.Name, false);
      until (FindNextUTF8(Info) <> 0)
    finally
      FindCloseUTF8(Info);
    end;
  if Recurse then
    if FindFirstUTF8(Directory + '*', faDirectory, Info) = 0 then
      try
        repeat
          if (Info.Name <> '.') and (Info.Name <> '') and (info.Name <> '..') and ((Info.Attr and faDirectory) <> 0) then
            AddDir(Directory + Info.Name, true);
        until (FindNextUTF8(Info) <> 0)
      finally
        FindCloseUTF8(Info);
      end;
end;

procedure TMainForm.AExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.OpenDirRecursivelyExecute(Sender: TObject);
// Open a directory recursively
var
  Dir: string;
  WasSorted: boolean;
begin
  if SelectDirectory(SSelectImageDirRec, '/', Dir, true) then
  begin
    Screen.BeginWaitCursor; //Show user he may have to wait for big directories
    try
      LBFiles.Items.BeginUpdate; //Indicate to the listbox that we're doing a lengthy operation
      WasSorted:=LBFiles.Sorted;
      LBFiles.Sorted:=true;
      AddDir(Dir, true);
      LBFiles.Sorted:=WasSorted;
    finally
      LBFiles.Items.EndUpdate;
      Screen.EndWaitCursor;
    end;
  end;
end;

procedure TMainForm.AClearExecute(Sender: TObject);
begin
  LBFiles.ItemIndex := -1;
  ShowFile(-1);
  LBFiles.Items.Clear;
end;

procedure TMainForm.ADoubleSizeExecute(Sender: TObject);

begin
  RescaleImage(2.0);
end;

procedure TMainForm.RescaleImage(NewScale: double);

var
  OrgWidth, OrgHeight: integer;
  Rect: TRect;

begin
  OrgWidth := IMain.Picture.Bitmap.Width;
  OrgHeight := IMain.Picture.Bitmap.Height;
  FImageScale := FImageScale * NewScale;
  Rect := IMain.BoundsRect;
  Rect.Right := Rect.Left + Round(OrgWidth * FImageScale);
  Rect.Bottom := Rect.Top + Round(OrgHeight * FImageScale);
  Imain.Align := AlNone;
  IMain.BoundsRect := Rect;
  Imain.Stretch := true;
end;

procedure TMainForm.AHalfSizeExecute(Sender: TObject);
begin
  RescaleImage(0.5);
end;

procedure TMainForm.NextImage;

begin
  ShiftImageIndex(1);
end;

procedure TMainForm.PreviousImage;

begin
  ShiftImageIndex(-1);
end;

procedure TMainForm.ShiftImageIndex(MoveBy: integer);

var
  ImageIndex: integer;

begin
  ImageIndex := LBFiles.ItemIndex;
  ImageIndex := ImageIndex + MoveBy;
  if ImageIndex < 0 then
    ImageIndex := LBFiles.Items.Count - 1;
  if ImageIndex >= LBFiles.Items.Count then
  begin
    ImageIndex := 0;
    if LBFiles.Items.Count = 0 then
      ImageIndex := -1;
  end;
  ShowFile(ImageIndex);
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  // todo: write help about with at least key combinations!
  if (shift = [ssShift]) or (shift = [ssAlt]) then
  begin
    if (key = VK_Prior) then
    begin
      // Page Up
      RescaleImage(2.0);
      Key := 0;
    end
    else if (key = VK_Next) then
    begin
      // Page Down
      RescaleImage(0.5);
      Key := 0;
    end
    else if (key = VK_Left) then
    begin
      // Left
      PreviousImage;
      Key := 0;
    end
    else if (key = VK_right) then
    begin
      // Right
      NextImage;
      Key := 0;
    end;
  end
  else if (shift = []) then
  begin
    if Key = VK_UP then
    begin
      // Up
      Previousimage;
      Key := 0;
    end
    else if Key = VK_DOWN then
    begin
      // Down
      NextImage;
      Key := 0;
    end;
  end;
end;

procedure TMainForm.DoError(Msg: string; Args: array of const);

begin
  ShowMessage(Format(Msg, Args));
end;

procedure TMainForm.ProcessCommandLine;

  function CheckOption(Index: integer; Short, Long: string): boolean;

  var
    O: string;

  begin
    O := ParamStrUTF8(Index);
    Result := (O = '-' + short) or (copy(O, 1, Length(Long) + 3) = ('--' + long + '='));
  end;

  function OptionArg(var Index: integer): string;

  var
    P: integer;

  begin
    if (Length(ParamStrUTF8(Index)) > 1) and (ParamStrUTF8(Index)[2] <> '-') then
    begin
      if Index < ParamCount then
      begin
        Inc(Index);
        Result := ParamStrUTF8(Index);
      end
      else
        DoError(SErrNeedArgument, [Index, ParamStrUTF8(Index)]);
    end
    else if length(ParamStrUTF8(Index)) > 2 then
    begin
      P := Pos('=', ParamStrUTF8(Index));
      if (P = 0) then
        DoError(SErrNeedArgument, [Index, ParamStrUTF8(Index)])
      else
      begin
        Result := ParamStrUTF8(Index);
        Delete(Result, 1, P);
      end;
    end;
  end;

var
  I: integer;
  S: string;
  FRecursive: boolean;

begin
  FRecursive := false;
  I := 0;
  while (I < ParamCount) do
  begin
    Inc(I);
    if CheckOption(I, 'r', 'recursive') then
      FRecursive := true
    else
    begin
      S := ParamStrUTF8(I);
      Screen.BeginWaitCursor; //Show user he may have to wait
      try
        if DirectoryExistsUTF8(S) then
          AddDir(ExpandFileNameUTF8(S), FRecursive)
        else if FileExistsUTF8(S) then
          AddFile(ExpandFileNameUTF8(S), LBFiles.Items.Count = 0);
      finally
        Screen.EndWaitCursor;
      end;
    end;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  ProcessCommandLine;
end;

procedure TMainForm.NextImageDir;

var
  Index: integer;

begin
  Index := NextDirIndex(1);
  ShowFile(Index);
end;

function TMainForm.NextDirIndex(Direction: integer): integer;

var
  Dir: string;

begin
  Result := -1;
  if LBFiles.ItemIndex = -1 then
    Exit;
  Result := LBFiles.ItemIndex;
  Dir := ExtractFilePath(LBFiles.Items[Result]);
  repeat
    Result := Result + Direction;
  until ((Result = -1) or (Result >= LBFiles.Items.Count)) or (Dir <> ExtractFilePath(LBFiles.Items[Result]));
  if Result >= LBFiles.Items.Count then
    Result := -1;
end;

procedure TMainForm.PreviousImageDir;
var
  Index: integer;

begin
  Index := NextDirIndex(-1);
  ShowFile(Index);
end;

procedure TMainForm.ANextImageExecute(Sender: TObject);
begin
  NextImage;
end;

procedure TMainForm.APreviousImageExecute(Sender: TObject);
begin
  PreviousImage;
end;

procedure TMainForm.ANextImageDirExecute(Sender: TObject);
begin
  NextImageDir;
end;

procedure TMainForm.APrevImageDirExecute(Sender: TObject);
begin
  PreviousImageDir;
end;

end.
