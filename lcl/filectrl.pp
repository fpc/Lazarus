{
 /***************************************************************************
                               filectrl.pp
                               -----------
                             Component Library File Controls
                   Initial Revision  : Sun Apr 23 18:30:00 PDT 2000


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

This unit contains file and directory controls and supporting handling functions. 
} 

unit FileCtrl;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  Classes, SysUtils,
  {$ifdef VER1_0}
  // fpc 1.0.x needs unit in which redefined property originally is defined
  // otherwise IE 55665566 occurs.
  Controls,
  {$endif}
  StdCtrls, FileUtil;

Type

  { TCustomFileListBox }

  TFileAttr = (ftReadOnly, ftHidden, ftSystem, ftVolumeID, ftDirectory,
               ftArchive, ftNormal);

  TFileType = set of TFileAttr;

  TCustomFileListBox = class(TCustomListBox)
  private
    FDrive: Char;
    FDirectory: String;
    FFileName: String;
    FFileType: TFileType;
    FMask: String;
    FOnChange: TNotifyEvent;
    FLastChangeFileName: string;
    function MaskIsStored: boolean;
    procedure SetDirectory(const AValue: String);
    procedure SetDrive(const AValue: Char);
    procedure SetFileName(const AValue: String);
    procedure SetFileType(const AValue: TFileType);
    procedure SetMask(const AValue: String);
    procedure UpdateSelectedFileName;
  protected
    procedure DoChangeFile; virtual;
    procedure UpdateFileList; virtual;
    procedure Click; override;
    procedure Loaded; override;
    function IndexOfFile(const AFilename: string): integer;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Drive: Char Read FDrive Write SetDrive default ' ';
    property Directory: String Read FDirectory Write SetDirectory;
    property FileName: String Read FFileName Write SetFileName;
    property FileType: TFileType Read FFileType Write SetFileType default [ftNormal];
    property Mask: String Read FMask Write SetMask stored MaskIsStored;
    property OnChange: TNotifyEvent Read FOnChange Write FOnChange;
    property Sorted default true;
  end;


  { TFileListBox }

  TFileListBox = class(TCustomFileListBox)
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Constraints;
    property ExtendedSelect;
    property FileType;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property Mask;
    property MultiSelect;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property ParentShowHint;
    property ParentFont;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TopIndex;
    property Visible;
  end;

procedure Register;

implementation

{ TCustomFileListBox }

procedure TCustomFileListBox.UpdateFileList;
Var
  Info: TSearchRec;
  Added: Boolean;

  procedure AddFile(FileAttr: TFileAttr; SysAttr: integer);
  begin
    if (not Added) and (FileAttr in FileType)
    and ((Info.Attr and SysAttr)>0) then begin
      if (Info.Attr and faDirectory)>0 then
        Info.Name := '['+Info.Name+']';
      Items.Add(Info.Name);
      Added:=true;
    end;
  end;

begin
  if [csloading,csdestroying]*ComponentState<>[] then exit;
  Clear;
  If SysUtils.FindFirst(FDirectory+DirectorySeparator+GetAllFilesMask,faAnyFile,
    Info)=0
  then
    Repeat
      if FileInFilenameMasks(Info.Name,Mask) then begin
        Added:=false;
        AddFile(ftReadOnly,faReadOnly);
        AddFile(ftHidden,faHidden);
        AddFile(ftSystem,faSysFile);
        AddFile(ftVolumeID,faVolumeId);
        AddFile(ftDirectory,faDirectory);
        AddFile(ftArchive,faArchive);
        AddFile(ftNormal,faArchive);
      end;
    Until SysUtils.FindNext(Info) <> 0;
  SysUtils.FindClose(Info);

  UpdateSelectedFileName;
end;

procedure TCustomFileListBox.Click;
begin
  UpdateSelectedFileName;
  inherited Click; 
end;

procedure TCustomFileListBox.Loaded;
begin
  inherited Loaded;
  UpdateFileList;
end;

function TCustomFileListBox.IndexOfFile(const AFilename: string): integer;
var
  CurItem: string;
begin
  Result:=0;
  while (Result<Items.Count) do begin
    CurItem:=Items[Result];
    if (CompareFilenames(AFilename,CurItem)=0)
    or ((CurItem<>'') and (CurItem[1]='[') and (CurItem[length(CurItem)]=']')
      and (CompareFilenames('['+AFilename+']',CurItem)=0))
    then
      exit;
    inc(Result);
  end;
  Result:=-1;
end;

procedure TCustomFileListBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  UpdateSelectedFileName;
  inherited KeyUp(Key, Shift);
end;

procedure TCustomFileListBox.SetFileType(const AValue: TFileType);
begin
  if FFileType=AValue then exit;
  FFileType := AValue;
  UpdateFileList;
end;

procedure TCustomFileListBox.SetDirectory(const AValue: String);
begin
  if FDirectory=AValue then exit;
  FDirectory := AValue;
  UpdateFileList;
end;

function TCustomFileListBox.MaskIsStored: boolean;
begin
  Result:=(FMask<>GetAllFilesMask);
end;

procedure TCustomFileListBox.SetDrive(const AValue: Char);
begin
  if FDrive=AValue then exit;
  FDrive := AValue;
  // ToDo: change to current directory of drive
  UpdateFileList;
end;

procedure TCustomFileListBox.SetFileName(const AValue: String);
var
  i: Integer;
begin
  i:=IndexOfFile(AValue);
  if i<>ItemIndex then begin
    ItemIndex:=i;
    UpdateSelectedFileName;
  end;
end;

procedure TCustomFileListBox.SetMask(const AValue: String);
begin
  if FMask = AValue then exit;
  FMask := AValue;
  UpdateFileList;
end;

procedure TCustomFileListBox.UpdateSelectedFileName;
var
  i: Integer;
begin
  i:=ItemIndex;
  if i<0 then
    FFileName := ''
  else begin
    FFileName := FDirectory+DirectorySeparator+Items[i];
    if (FFileName<>'')
    and (FFileName[1]='[') and (FFileName[length(FFileName)]=']') then
      FFileName:=copy(FFileName,2,length(FFileName)-2);
  end;
  DoChangeFile;
end;

procedure TCustomFileListBox.DoChangeFile;
begin
  if FFilename=FLastChangeFileName then exit;
  FLastChangeFileName:=FFilename;
  If Assigned(FOnChange) then FOnChange(Self);
end;

constructor TCustomFileListBox.Create(TheOwner: TComponent);
var
  FileDrive: String;
begin
  inherited Create(TheOwner);
  //Initializes the Mask property.
  FMask := GetAllFilesMask;
  //Initializes the FileType property.
  FFileType := [ftNormal];
  //Initializes the Directory and Drive properties to the current directory.
  FDirectory := GetCurrentDir;
  FileDrive := ExtractFileDrive(FDirectory);
  if FileDrive<>'' then
    FDrive:=FileDrive[1]
  else
    FDrive:=' ';
  //Initializes the MultiSelect property.
  MultiSelect := False;
  //Fills the list box with all the files in the directory.
  UpdateFileList;
  //Initializes the Sorted property.
  Sorted := True;
end;

destructor TCustomFileListBox.Destroy;
begin
  inherited Destroy; 
end;

procedure Register;
begin
  RegisterComponents('Misc',[TFileListBox]);
end;

end.

{
  $Log$
  Revision 1.33  2005/03/23 10:45:06  mattias
  fixed ambigious with ambiguous

  Revision 1.32  2004/11/15 22:58:13  mattias
  implemented Multi mask for TFileListBox

  Revision 1.31  2004/09/27 21:45:44  vincents
  splitted off unit FileUtil, it doesn't depend on other LCL units

  Revision 1.30  2004/09/27 10:35:51  vincents
  Added CopyFile with PerserveTime parameter

  Revision 1.29  2004/09/01 09:43:24  mattias
  implemented registration of project file types

  Revision 1.28  2004/08/22 22:47:43  mattias
  implemented context help for source editor

  Revision 1.27  2004/04/21 21:22:52  mattias
  fixed updatinf Filename when setting Filename  from Luis

  Revision 1.26  2004/04/21 17:41:29  mattias
  added directory brackets  from Luis

  Revision 1.25  2004/03/12 15:48:57  mattias
  fixed 1.0.x compilation

  Revision 1.24  2004/03/12 12:53:51  vincents
  fixed comment

  Revision 1.23  2004/03/12 11:54:57  vincents
  fixed 1.0.x compilation, removed double constants

  Revision 1.22  2004/03/11 00:07:26  mattias
  added TFileListBox  from Luis

  Revision 1.21  2004/01/23 19:36:49  mattias
  fixed searching dir in searchpath under win32

  Revision 1.20  2003/12/21 13:58:06  mattias
  renamed DirectoryExists to DirPathExists to reduce ambiguousity

  Revision 1.19  2003/10/31 14:25:59  mazen
  * Fixing VER1_1 compile problem to allow using 1.1 compiler
  * Most of oldlinux unit calls are now in BaseUnix unit with prefix Fp

  Revision 1.18  2003/09/02 21:32:56  mattias
  implemented TOpenPictureDialog

  Revision 1.17  2003/08/13 16:18:58  mattias
  started check compiler options

  Revision 1.16  2003/03/29 17:20:05  mattias
  added TMemoScrollBar

  Revision 1.15  2003/03/28 23:03:38  mattias
  started TMemoScrollbar

  Revision 1.14  2003/03/26 11:39:08  mattias
  fixed rtl include path

  Revision 1.13  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.12  2003/02/20 11:03:20  mattias
  save as of project files now starts in project dierctory

  Revision 1.11  2003/02/07 17:49:21  mattias
  added ReadAllLinks

  Revision 1.10  2003/01/17 16:28:42  mattias
  updated translation files

  Revision 1.9  2002/12/23 13:20:46  mattias
  fixed backuping symlinks

  Revision 1.8  2002/12/23 10:12:40  mattias
  added symlink test and unit types

  Revision 1.7  2002/12/17 19:49:34  mattias
  finished publish project

  Revision 1.6  2002/12/12 17:47:44  mattias
  new constants for compatibility

  Revision 1.5  2002/12/09 16:48:36  mattias
  added basic file handling functions to filectrl

  Revision 1.4  2002/05/29 21:44:38  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.3  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.2  2001/06/15 10:31:05  lazarus
  MG: set longstrings as default

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.2  2000/05/09 00:00:33  lazarus
  Updated my email address in the documentation to the current one. Also
  removed email references in comments that were not @author comments to
  fix problems with the documentation produced by pasdoc.           CAW

  Revision 1.1  2000/04/24 05:03:25  lazarus
  Added filectrl unit for DirectoryExists function.      CAW


}

