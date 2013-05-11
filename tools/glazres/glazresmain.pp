unit glazresmain;

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

  Author of LazRes: Mattias Gaertner
  Original idea for GLazRes: Andy Koz
  Adapted by: Bart Broersma

  GLazRes aims to be a GUI implementation of the LazRes program.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLProc, LResources, Buttons, EditBtn, LazUtf8Classes, Types, LCLType,
  ExtDlgs, IniFiles;

type

  { TGLazResForm }

  TGLazResForm = class(TForm)
    ClearBtn: TBitBtn;
    CloseBtn: TBitBtn;
    DestEdt: TFileNameEdit;
    MessagesLabel: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    AddImgBtn: TBitBtn;
    StartBtn: TBitBtn;
    FilesLabel: TLabel;
    FileListBox: TListBox;
    MsgMemo: TMemo;
    AddAnyBtn: TBitBtn;
    LrsLabel: TLabel;
    OpenDialog: TOpenDialog;
    DeleteBtn: TBitBtn;
    procedure AddImgBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure DestEdtAcceptFileName(Sender: TObject; var {%H-}Value: String);
    procedure DestEdtEditingDone(Sender: TObject);
    procedure FileListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure AddAnyBtnClick(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
  private
    { private declarations }
    FIniFileName: String;
    procedure CreateAnchors;
    procedure ResizeControls({%H-}Dummy: PtrInt);
    procedure CreateLazarusResourceFile;
    procedure ConvertFormToText(Stream: TMemoryStream);
    procedure AddFiles(Names: TStrings);
    procedure MaybeEnableButtons;
    procedure AddMessage(const Msg: String);
    procedure AddMessageFmt(const Msg: String; Args: Array of const);
    procedure ClearMessages;
    procedure LoadWindowGeometry;
    procedure SaveWindowGeometry;
  public

  end;

var
  GLazResForm: TGLazResForm;


implementation

{$R *.lfm}

const
  ErrConvertToText = 'ERROR: unable to convert Delphi form to text: "%s"';
  ErrFileNotFound = 'ERROR: File not found: "%s"';
  ErrFileIsResource = 'ERROR: Cannot add resource file to itself ("%s")';
  ErrCreate = 'ERROR: Cannot create "%s"';
  ErrNoResourceName = 'ERROR: No resourcename found for "%s"';
  MsgProcessing = 'Processing "%s"';
  MsgResourceNameType = ' Resource name = "%s", Type = "%s"';
  ErrRead = 'ERROR: Cannot read from "%s"';
  MsgSuccess = 'Done.'+ LineEnding + 'Number of resources added: %d.';

  MsgWrongExt = 'Filename does not have the required extension: fix it?';

  AppName = 'GLazRes';
  IniName = {$ifdef windows}'GLazRes.ini'{$else}'glazres.conf'{$endif};
  scPosition = 'Position';
  idLeft = 'Left';
  idTop = 'Top';
  idWidth = 'Width';
  idHeight = 'Height';

//Needed for GetAppConfigDir
function GetVendorName: String;
begin
  Result := '';
end;

function GetAppName: String;
begin
  Result := AppName;
end;

{TGLazResForm}

// *************** Component Events *********************** //

procedure TGLazResForm.FormCreate(Sender: TObject);
begin
  OnGetVendorName := @GetVendorName;
  OnGetApplicationName := @GetAppName;
  FIniFileName := GetAppConfigDir(False) + IniName;
  CreateAnchors;
  LoadWindowGeometry;
end;

procedure TGLazResForm.FormShow(Sender: TObject);
begin
  MaybeEnableButtons;
  OnResize := @FormResize;
  //Using QueueAsyncCall delays the layout until the form is shown,
  //before that ClientWidht may have wrong value (depending on widgetset and windowmanager)
  Application.QueueAsyncCall(@ResizeControls,0);
end;

procedure TGLazResForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveWindowGeometry;
end;


procedure TGLazResForm.FormResize(Sender: TObject);
begin
  ResizeControls(0);
end;


procedure TGLazResForm.DestEdtAcceptFileName(Sender: TObject; var Value: String);
begin
  DestEdtEditingDone(DestEdt);
end;

procedure TGLazResForm.DestEdtEditingDone(Sender: TObject);
var
  Fn, Ext: String;
begin
  Fn := DestEdt.FileName;
  Ext := ExtractFileExt(Fn);
  if (Fn <> '') and (CompareText(Ext, '.lrs') <> 0) then
  begin
    if MessageDlg(AppName,MsgWrongExt,mtConfirmation,[mbYes,mbNo],0) = mrYes then
    begin
      Fn := ChangeFileExt(Fn, '.lrs');
      DestEdt.FileName := Fn;
    end;
  end;
  MaybeEnableButtons;
end;


procedure TGLazResForm.StartBtnClick(Sender: TObject);
begin
  CreateLazarusResourceFile;
end;


procedure TGLazResForm.AddAnyBtnClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    AddFiles(OpenDialog.Files);
  end;
end;

procedure TGLazResForm.AddImgBtnClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    AddFiles(OpenPictureDialog.Files);
  end;
end;


procedure TGLazResForm.DeleteBtnClick(Sender: TObject);
var
  Index: integer;
begin
  for Index := FileListBox.Count - 1 downto 0 do
  begin
    if FileListBox.Selected[Index] then
      FileListBox.Items.Delete(Index);
  end;
  MaybeEnableButtons;
end;


procedure TGLazResForm.ClearBtnClick(Sender: TObject);
begin
  FileListBox.Items.Clear;
  MaybeEnableButtons;
end;


procedure TGLazResForm.FileListBoxDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  OldBrushStyle: TBrushStyle;
  OldTextStyle: TTextStyle;
  NewTextStyle: TTextStyle;
  ItemText: String;
  TheCanvas: TCanvas;
begin
  //Objective: draw only the FileName, not the fully qualified path.
  if (odPainted in State) then Exit;
  TheCanvas := (Control as TCustomListBox).Canvas;

  ItemText := ExtractFileName(FileListBox.Items[Index]);

  TheCanvas.FillRect(ARect);

  OldBrushStyle := TheCanvas.Brush.Style;
  TheCanvas.Brush.Style := bsClear;

  OldTextStyle := TheCanvas.TextStyle;
  NewTextStyle := OldTextStyle;
  NewTextStyle.Layout := tlCenter;
  NewTextStyle.RightToLeft := Control.UseRightToLeftReading;
  if Control.UseRightToLeftAlignment then
  begin
    NewTextStyle.Alignment := taRightJustify;
    ARect.Right := ARect.Right - 2;
  end
  else
  begin
    NewTextStyle.Alignment := taLeftJustify;
    ARect.Left := ARect.Left + 2;
  end;

  TheCanvas.TextStyle := NewTextStyle;

  TheCanvas.TextRect(ARect, ARect.Left, ARect.Top, ItemText);
  TheCanvas.Brush.Style := OldBrushStyle;
  TheCanvas.TextStyle := OldTextStyle;
end;




// ***************** Form layout and looks *********************** //

procedure TGLazResForm.CreateAnchors;
begin
  DestEdt.AnchorToNeighbour(akTop, 5, LrsLabel);
  FilesLabel.AnchorToNeighbour(akTop, 10, DestEdt);
  FileListBox.AnchorToNeighbour(akTop, 5, FilesLabel);
  AddAnyBtn.AnchorToNeighbour(akTop, 10, FileListBox);
  AddImgBtn.AnchorToNeighbour(akTop, 10, FileListBox);
  DeleteBtn.AnchorToNeighbour(akTop, 10, FileListBox);
  ClearBtn.AnchorToNeighbour(akTop, 10, FileListBox);
  AddImgBtn.AnchorToNeighbour(akLeft, 5 , AddAnyBtn);
  DeleteBtn.AnchorToNeighbour(akLeft, 15, AddImgBtn);
  ClearBtn.AnchorToNeighbour(akLeft, 5, DeleteBtn);
  MessagesLabel.AnchorToNeighbour(akTop, 10, AddAnyBtn);
  MsgMemo.AnchorToNeighbour(akTop, 5, MessagesLabel);
  StartBtn.AnchorToNeighbour(akTop, 10, MsgMemo);
  CloseBtn.AnchorToNeighbour(akTop, 10, MsgMemo);
  CloseBtn.AnchorToNeighbour(akLeft, 5, StartBtn);
end;



procedure TGLazResForm.ResizeControls(Dummy: PtrInt);
var
  CH, CW, LMargin, MinW, MinH, HeightLeft: Integer;
begin
  CH := ClientHeight;
  CW := ClientWidth;
  LMargin := LrsLabel.Left;
  MinW :=  ClearBtn.Left + ClearBtn.Width + 2 * LMargin;
  DestEdt.Width := CW - (2 * LMargin) - DestEdt.ButtonWidth;
  if (DestEdt.Width < MinW) then DestEdt.Width := MinW;
  FileListBox.Width := CW - (2 * LMargin);
  if (FileListBox.Width < MinW) then FileListBox.Width := MinW;
  MsgMemo.Width := FileListBox.Width;
  StartBtn.Left := CW - (StartBtn.Width + CloseBtn.Width + 5) - LMargin;

  MinH := 532; //desing time value
  if CH <= MinH then
  begin
    MsgMemo.Height := 138;// design time value;
    Exit;
  end;
  HeightLeft := CH - MsgMemo.Top;
  MsgMemo.Height := HeightLeft - StartBtn.Height - 10 - 10;
end;


procedure TGLazResForm.LoadWindowGeometry;
var
  IniDir: String;
  Ini: TIniFile;
  L, T, W, H: LongInt;
begin
  IniDir := ExtractFileDir(FIniFileName);
  if not DirectoryExists(IniDir) then if not ForceDirectories(IniDir) then Exit;
  try
    Ini := TIniFile.Create(FIniFileName);
    try
      L := Ini.ReadInteger(scPosition, idLeft, Left);
      T := Ini.ReadInteger(scPosition, idTop, Top);
      W := Ini.ReadInteger(scPosition, idWidth, Width);
      H := Ini.ReadInteger(scPosition, idHeight, Height);
      SetBounds(L, T, W, H);
    finally
      Ini.Free;
    end;
  except
    debugln('Error reading geometry from "',FIniFileName,'".');
  end;
end;

procedure TGLazResForm.SaveWindowGeometry;
var
  IniDir: String;
  Ini: TIniFile;
begin
  IniDir := ExtractFileDir(FIniFileName);
  if not DirectoryExists(IniDir) then if not ForceDirectories(IniDir) then
  begin
    debugln('Unable to create config file "',FIniFileName,'".');
    Exit;
  end;
  try
    Ini := TIniFile.Create(FIniFileName);
    try
      Ini.CacheUpdates := True;
      Ini.WriteInteger(scPosition, idLeft, Left);
      Ini.WriteInteger(scPosition, idTop, Top);
      Ini.WriteInteger(scPosition, idWidth, Width);
      Ini.WriteInteger(scPosition, idHeight, Height);
    finally
      Ini.Free;
    end;
  except
    debugln('Error saving geometry to "',FIniFileName,'".');
  end;
end;

procedure TGLazResForm.MaybeEnableButtons;
begin
  StartBtn.Enabled := (DestEdt.FileName <> '') and
                      (FileListBox.Count > 0);
  DeleteBtn.Enabled := (FileListBox.Count > 0);
  ClearBtn.Enabled := (FileListBox.Count > 0);
end;


// ************** LRS Creating related procedures ***************** //

procedure TGLazResForm.AddFiles(Names: TStrings);
var
  Index: Integer;
begin
  for Index := 0 to Names.Count - 1 do
  begin
    FileListBox.Items.Add(Names[Index]);
  end;
  MaybeEnableButtons;
end;



procedure TGLazResForm.ConvertFormToText(Stream: TMemoryStream);
var TextStream: TMemoryStream;
begin
  try
    try
      TextStream:=TMemoryStream.Create;
      FormDataToText(Stream,TextStream);
      TextStream.Position:=0;
      Stream.Clear;
      Stream.CopyFrom(TextStream,TextStream.Size);
      Stream.Position:=0;
    except
      on E: Exception do begin
        debugln(Format(ErrConvertToText,[E.Message]));
      end;
    end;
  finally
    TextStream.Free;
  end;
end;


procedure TGLazResForm.CreateLazarusResourceFile;
var
  FileCount, Index:integer;
  S:string;
  ResFileStream, BinFileStream: TFileStreamUtf8;
  ResMemStream, BinMemStream: TMemoryStream;
  ResourceFilename, FullResourceFilename, BinFilename, BinExt, ResourceName, ResourceType: String;
begin
  FileCount := FileListBox.Count;
  if FileCount = 0 then
    Exit;

  FullResourceFileName := ExpandFileNameUtf8(DestEdt.FileName);
  ResourceFileName := ExtractFileName(FullResourceFileName);
  ClearMessages;

  FullResourceFilename := ExpandFileNameUTF8(ResourceFilename);
  // check that all resources exists and are not the destination file
  for Index := 0 to FileCount-1 do
  begin
    S := FileListBox.Items[Index]; //FileListBox[Index];
    if not FileExistsUTF8(S) then
    begin
      AddMessageFmt(ErrFileNotfound,[S]);
      exit;
    end;
    if CompareFileNames(ExpandFileNameUTF8(S), FullResourceFilename, True) = 0 then
    begin
      AddMessageFmt(ErrFileIsResource,[S]);
      exit;
    end;
  end;
  try
    ResFileStream:=TFileStreamUtf8.Create(ResourceFilename,fmCreate);
  except
    AddMessageFmt(ErrCreate,[ResourceFileName]);
    exit;
  end;
  ResMemStream:=TMemoryStream.Create;
  try
    for Index := 0 to FileCount - 1 do
    begin
      BinFilename:=FileListBox.Items[Index];
      AddMessageFmt(MsgProcessing,[BinFilename]);
      try
        BinFileStream:=TFileStreamUtf8.Create(BinFilename, fmOpenRead);
        BinMemStream:=TMemoryStream.Create;
        try
          BinMemStream.CopyFrom(BinFileStream, BinFileStream.Size);
          BinMemStream.Position := 0;
          BinExt := Utf8UpperCase(ExtractFileExt(BinFilename));
          if (BinExt='.LFM') or (BinExt='.DFM') or (BinExt='.XFM')
          then
          begin
            ResourceType:='FORMDATA';
            ConvertFormToText(BinMemStream);
            ResourceName:=FindLFMClassName(BinMemStream);
            if ResourceName='' then
            begin
              AddMessageFmt(ErrNoResourceName,[BinFileName]);
              exit;
            end;
            AddMessageFmt(MsgResourceNameType,[ResourceName,ResourceType]);
            LFMtoLRSstream(BinMemStream,ResMemStream);
          end
          else
          begin
            ResourceType := copy(BinExt,2,length(BinExt)-1);
            ResourceName := ExtractFileName(BinFilename);
            ResourceName := copy(ResourceName, 1, Length(ResourceName) - Length(BinExt));
            if (ResourceName = '') then
            begin
              AddMessageFmt(ErrNoResourceName,[BinFileName]);
              exit;
            end;
            AddMessageFmt(MsgResourceNameType,[ResourceName,ResourceType]);
            BinaryToLazarusResourceCode(BinMemStream, ResMemStream ,ResourceName, ResourceType);
          end;
        finally
          BinFileStream.Free;
          BinMemStream.Free;
        end;
      except
        AddMessageFmt(ErrRead,[BinfileName]);
        exit;
      end;
    end;
    ResMemStream.Position := 0;
    ResFileStream.CopyFrom(ResMemStream, ResMemStream.Size);
    AddMessageFmt(MsgSuccess,[FileCount]);
  finally
    ResMemStream.Free;
    ResFileStream.Free;
  end;
end;

// ****************  User interaction **************** //

procedure TGLazResForm.AddMessage(const Msg: String);
begin
  MsgMemo.Lines.Add(Msg);
end;

procedure TGLazResForm.AddMessageFmt(const Msg: String; Args: array of const);
begin
  MsgMemo.Lines.Add(Format(Msg, Args));
end;

procedure TGLazResForm.ClearMessages;
begin
  MsgMemo.Lines.Clear;
end;


end.

