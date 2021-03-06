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

  Author of LazRes: Mattias Gaertner
  Original idea for GLazRes: Andy Koz
  Adapted by: Bart Broersma

  GLazRes aims to be a GUI implementation of the LazRes program.
}

unit glazresmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, IniFiles,
  Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtDlgs, EditBtn,
  LResources, LCLProc, LCLType, ExtCtrls,
  LazFileUtils, LazUTF8;

type

  { TGLazResForm }

  TGLazResForm = class(TForm)
    ClearBtn: TBitBtn;
    CloseBtn: TBitBtn;
    DestEdt: TFileNameEdit;
    Images: TImageList;
    MessagesLabel: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    AddImgBtn: TBitBtn;
    Splitter: TSplitter;
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
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);
    procedure AddAnyBtnClick(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
  private
    FIniFileName: String;
    procedure ResizeControls;
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

resourcestring
  ErrConvertToText = 'ERROR: unable to convert Delphi form to text: "%s"';
  ErrFileNotFound = 'ERROR: File not found: "%s"';
  ErrFileIsResource = 'ERROR: Cannot add resource file to itself ("%s")';
  ErrCreate = 'ERROR: Cannot create "%s"';
  ErrNoResourceName = 'ERROR: No resourcename found for "%s"';
  MsgCreatingLrs = 'Creating "%s"';
  MsgProcessing = 'Processing "%s"';
  MsgResourceNameType = 'Resource name = "%s", Type = "%s"';
  ErrRead = 'ERROR: Cannot read from "%s"';
  MsgSuccess = 'Done.'+ LineEnding + 'Number of resources added: %d.';

  MsgWrongExt = 'Filename does not have the required extension: fix it?';

  DESaveResourcefileAs = 'Save resourcefile as';
  DEFilter = 'Lazarus Resource Files|*.lrs|All Files|*';
  ODOpenExistingFile = 'Open existing file';
  OPDOpenExistingPicture = 'Open existing picture';
  OPDFilter ='Graphic (*.png;*.xpm;*.bmp;*.cur;*.ico;*.icns;*.jpeg;*.jpg;*.jpe;*.jfif;*.tif;*.tiff;*.gif;*.pbm;*.pgm;*.ppm;*.gif;*.tga)|*.png;*.xpm;*.bmp;*.cur;*.ico;*.icns;*.jpeg;*.jpg;*.jpe;*.jfif;*.tif;*.tiff;*.gif;*.pbm;*.pgm;*.ppm;*.gif;*.tga|Portable Network Graphic (*.png)|*.png|Pixmap (*.xpm)|*.xpm|Bitmaps (*.bmp)|*.bmp|Cursor (*.cur)|*.cur|Icon (*.ico)|*.ico|macOS Icon (*.icns)|*.icns|Joint Picture Expert Group (*.jpeg;*.jpg;*.jpe;*.jfif)|*.jpeg;*.jpg;*.jpe;*.jfif|Tagged Image File Format (*.tif;*.tiff)|*.tif;*.tiff|Graphics Interchange Format (*.gif)|*.gif|Portable PixMap (*.pbm;*.pgm;*.ppm)|*.pbm;*.pgm;*.ppm|Animated GIF (*.gif)|*.gif|TGA Image File (*.tga)|*.tga|';
  OPDFilterAll = 'All files';
  CBtnCancel = 'Close';

const
  AppName = 'GLazRes';
  IniName = {$ifdef windows}'GLazRes.ini'{$else}'glazres.conf'{$endif};
  scPosition = 'Position';
  idLeft = 'Left';
  idTop = 'Top';
  idWidth = 'Width';
  idHeight = 'Height';
  idSplitter = 'Splitter';

//Needed for GetAppConfigDir
function GetVendorName: String;
begin
  Result := '';
end;

function GetAppName: String;
begin
  Result := AppName;
end;

function MaxValue(const Data: array of integer): Integer;
var
  value: Integer;
begin
  Result := -MaxInt;
  for value in Data do
    if value > Result then Result := value;
end;


{TGLazResForm}

// *************** Component Events *********************** //

procedure TGLazResForm.FormCreate(Sender: TObject);
begin
  OnGetVendorName := @GetVendorName;
  OnGetApplicationName := @GetAppName;
  FIniFileName := GetAppConfigDir(False) + IniName;
  LoadWindowGeometry;
  DestEdt.DialogTitle := DESaveResourcefileAs;
  DestEdt.Filter := DEFilter;
  OpenDialog.Title := ODOpenExistingFile;
  //OpenDialog.Filter := OPDFilterAll + {$IFDEF WINDOWS} ' (*.*)|*.*|' {$ELSE} ' (*)|*|' {$ENDIF} ;
  OpenPictureDialog.Title := OPDOpenExistingPicture;
  OpenPictureDialog.Filter := OPDFilter + OPDFilterAll + {$IFDEF WINDOWS} ' (*.*)|*.*|' {$ELSE} ' (*)|*|' {$ENDIF} ;
  CloseBtn.Caption := CBtnCancel;
end;

procedure TGLazResForm.FormDropFiles(Sender: TObject; 
  const FileNames: array of string);
var 
  Files: TStringList = nil;
  i: Integer;
begin
  Files := TStringList.Create;
  try
    for i := 0 to High(FileNames) do
      Files.Add(Filenames[i]);
    if Files.Count > 0 then
      AddFiles(Files);
  finally
    Files.Free;
  end;
  MaybeEnableButtons;
end;

procedure TGLazResForm.FormShow(Sender: TObject);
begin
  MaybeEnableButtons;
  ResizeControls;
end;

procedure TGLazResForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveWindowGeometry;
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
  TheCanvas := (Control as TCustomListBox).Canvas;

  ItemText := ExtractFileName(FileListBox.Items[Index]);

  if not(odBackgroundPainted in State) then
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

procedure TGLazResForm.ResizeControls;
var
  w: Integer;
begin
  w := MaxValue([CloseBtn.Width, StartBtn.Width]);
  CloseBtn.Constraints.MinWidth := w;
  StartBtn.Constraints.MinWidth := w;
  
  Constraints.MinWidth := ClearBtn.Left + ClearBtn.Width + AddAnyBtn.Left;
  Constraints.MinHeight := Constraints.MinWidth;
  
  if Width < Constraints.MinWidth then Width := Constraints.MinWidth;     // enforce constraints
  if Height < Constraints.MinHeight then Height := Constraints.MinHeight;
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
      Scaled := false;   // Avoid double scaling when exe created at 96 ppi is run at higher ppi
      SetBounds(L, T, Scale96ToForm(W), Scale96ToForm(H));  // manually scale from stored size at 96 ppi
      Splitter.Top := Scale96ToForm(Ini.ReadInteger(scPosition, idSplitter, Splitter.Top));
      Scaled := true;
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
      Ini.WriteInteger(scPosition, idWidth, ScaleFormTo96(Width));   // Store size for 96 ppi
      Ini.WriteInteger(scPosition, idHeight, ScaleFormTo96(Height));
      Ini.WriteInteger(scPosition, idSplitter, ScaleFormTo96(Splitter.Top));
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
  ResFileStream, BinFileStream: TFileStream;
  ResMemStream, BinMemStream: TMemoryStream;
  ResourceFilename, FullResourceFilename, BinFilename, BinExt, ResourceName, ResourceType,
    ExpS: String;
begin
  FileCount := FileListBox.Count;
  if FileCount = 0 then
    Exit;

  FullResourceFileName := ExpandFileNameUtf8(DestEdt.FileName);
  ResourceFileName := ExtractFileName(FullResourceFileName);
  ClearMessages;

  // check that all resources exists and are not the destination file
  for Index := 0 to FileCount-1 do
  begin
    S := FileListBox.Items[Index]; //FileListBox[Index];
    if not FileExistsUTF8(S) then
    begin
      AddMessageFmt(ErrFileNotfound,[S]);
      exit;
    end;
    ExpS:=ExpandFileNameUTF8(S);
    if (CompareText(ExpS,FullResourceFilename)=0)
      or (CompareFilenamesIgnoreCase(ExpandFileNameUTF8(S), FullResourceFilename) = 0) then
    begin
      AddMessageFmt(ErrFileIsResource,[S]);
      exit;
    end;
  end;
  try
    AddMessageFmt(MsgCreatingLrs,[FullResourceFilename]);
    ResFileStream:=TFileStream.Create(FullResourceFileName,fmCreate);
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
        BinFileStream:=TFileStream.Create(BinFilename, fmOpenRead);
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

