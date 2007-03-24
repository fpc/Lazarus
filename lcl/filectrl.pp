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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  Classes, SysUtils, StdCtrls, FileUtil;

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
    procedure Click; override;
    procedure Loaded; override;
    function IndexOfFile(const AFilename: string): integer;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateFileList; virtual;
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
    property DragCursor;
    property DragMode;
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
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
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
    property OnStartDrag;
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
  If SysUtils.FindFirst(FDirectory+DirectorySeparator+AllDirectoryEntriesMask,
    faAnyFile, Info)=0
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
        if not Added and (ftNormal in FileType) and
          (faAnyFile and Info.Attr=0) then
          Items.Add(Info.Name);
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
  Result:=(FMask<>AllDirectoryEntriesMask);
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
  // in a multiselect listbox, the itemindex can be 0 in an empty list
  if (i<0) or (i>=Items.Count) then
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
  FMask := AllDirectoryEntriesMask;
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

