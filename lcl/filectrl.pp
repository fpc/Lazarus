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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  Classes, SysUtils, StdCtrls, FileUtil, Masks, Graphics;

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
    property Directory;
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

function MiniMizeName(FileName: String; Canvas: TCanvas; MaxWidth: Integer): String;

procedure Register;

implementation


function MiniMizeName(FileName: String; Canvas: TCanvas; MaxWidth: Integer): String;
{
 This function will return a shortened version of FileName, so that it fits
 on the given Canvas, with a given MaxWidth.
 eg. C:\Documents and Settings\User\Application Data\Microsoft\Word\custom.dic
     would become something like: C:\...\Word\custom.dic
}
  procedure RemoveFirstDir(var Dir: String);
  {
   This procedure will remove the first directory from Dir
   and will set ADelim to the Delimiter that separated the first Dir
   eg. In: Dir: 'Dir1\Dir2\Dir3'
  }
  var p: Integer;
  begin
    p:= Pos(PathDelim,Dir);
    if (p > 0) then
    begin
      Dir := Copy(Dir,p+1,Length(Dir)-p);
    end;
  end;
var Drive, Dir, Fn: String;
    ComposedName: String;
    TWidth: Integer;
begin
  Result := FileName;
  //if FileName does not contain any (sub)dir then return FileName
  if Pos(PathDelim, FileName) = 0 then Exit;
  //if FileName fits, no need to do anyhing
  if Canvas.TextWidth(FileName) <= MaxWidth then Exit;
  Drive := ExtractFileDrive(FileName);
  Fn := ExtractFileName(FileName);
  Dir := ExtractFilePath(FileName);
  //Remove Drive from Dir
  if (Length(Drive) > 0) then System.Delete(Dir, 1, Length(Drive));
  //Transfer all PathDelimiters at the start of Dir to Drive
  While (Length(Dir) > 0) and (Dir[1] in ['/','\']) do
  begin
    Drive := Drive + Dir[1];
    System.Delete(Dir,1,1);
  end;
  //if Dir is empty then we cannot shorten it,
  //and we know at this point that Drive+FileName is too long, so we return only filename
  if (Length(Dir) = 0) then
  begin
    Result := Fn;
    Exit;
  end;
  repeat
    //at this point we know that Dir ends with PathDelim (otherwise we exited before this point,
    //so RemoveFirstDir will return a truncated Dir or an empty string
    RemoveFirstDir(Dir);
    ComposedName := Drive+'...'+PathDelim+Dir+Fn;
    TWidth := Canvas.TextWidth(ComposedName);
  until (Length(Dir) = 0) or (TWidth <= MaxWidth);
  if (TWidth <= MaxWidth) then Result := ComposedName else Result := Fn;
end;




{ TCustomFileListBox }

procedure TCustomFileListBox.UpdateFileList;
const
  AttrNotNormal = faReadOnly or
                  faHidden or
                  faSysFile or
                  faVolumeID or
                  faDirectory or
                  faArchive;
var
  Info: TSearchRec;

  function FileTypeToFileAttribute(FileType: TFileType): LongInt;
  const
    FileTypeToAttrMap: array[TFileAttr] of LongInt =
    (
 { ftReadOnly  } faReadOnly,
 { ftHidden    } faHidden,
 { ftSystem    } faSysFile,
 { ftVolumeID  } faVolumeId,
 { ftDirectory } faDirectory,
 { ftArchive   } faArchive,
 { ftNormal    } 0
    );
  var
    Iter: TFileAttr;
  begin
    Result := 0;
    for Iter := Low(TFileAttr) to High(TFileAttr) do
      if Iter in FileType then
        Result := Result or FileTypeToAttrMap[Iter];
  end;

begin
  if [csloading, csdestroying] * ComponentState <> [] then
    Exit;
  Clear;
  if FileType <> [] then
  begin
    if FindFirstUTF8(
      IncludeTrailingPathDelimiter(FDirectory)+AllDirectoryEntriesMask,
      FileTypeToFileAttribute(FileType), Info) = 0
    then
      repeat
        if MatchesMaskList(Info.Name,Mask) then
        begin
          if (ftNormal in FileType) or ((Info.Attr and AttrNotNormal) > 0) then
          begin
            if (Info.Attr and faDirectory) > 0 then
              Items.Add('['+Info.Name+']')
            else
              Items.Add(Info.Name);
          end;
        end;
      until FindNextUTF8(Info) <> 0;
    FindCloseUTF8(Info);
  end;

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
  FileDrive: string;
  CurrentDir: string;
begin
  inherited Create(TheOwner);
  //Initializes the Mask property.
  FMask := AllDirectoryEntriesMask;
  //Initializes the FileType property.
  FFileType := [ftNormal];
  //Initializes the Directory and Drive properties to the current directory.
  CurrentDir := GetCurrentDirUTF8;
  FDirectory := CurrentDir;
  FileDrive := ExtractFileDrive(CurrentDir);
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

