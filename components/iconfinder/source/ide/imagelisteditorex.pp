{
 *****************************************************************************
  This file is part of a Lazarus Package, IconFinder.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 Extended image list component editor for searching icons by keywords.

 Each icon folder contained in the icon lib must have a file "metadata.xml" with
 a metadata block for each image.
}

unit ImageListEditorEx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, LazLoggerBase,
  // LCL
  Forms, Controls, Graphics, Dialogs, ImgList, ComCtrls, Menus, ActnList,
  // IDEIntf
  PropEdits, ComponentEditors, ImageListEditor, ObjInspStrConsts, IDEImagesIntf,
  // Thumbnails
  IconFinderStrConstsIDE, IconThumbnails, IconFinderFrm;

type

  { TImageListEditorDlgEx }

  TImageListEditorDlgEx = class(TImageListEditorDlg)
    procedure FormCreate(Sender: TObject);
  private
    FIconFinderForm: TIconFinderForm;
    procedure AddReplaceImgHandler(Sender: TObject);
    procedure AddReplaceFromIconFinder(AIconFinder: TIconFinderForm; AReplace: Boolean);
    function CreateIconFinder: TIconFinderForm;
    procedure IconFinderDblClick(Sender: TObject);
  end;


  { TImageListComponentEditorEx }

  TImageListComponentEditorEx = class(TComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb({%H-}Index: Integer); override;
    function GetVerb({%H-}Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;


implementation

{$R *.lfm}

function EditImageList(AImageList: TImageList): Boolean;
var
  ImageListEditorDlg: TImageListEditorDlgEx;
begin
  ImageListEditorDlg := TImageListEditorDlgEx.Create(Application);
  try
    ImageListEditorDlg.LoadFromImageList(AImageList);

    if ImageListEditorDlg.ShowModal = mrOk then
      ImageListEditorDlg.SaveToImageList;

    Result := ImageListEditorDlg.Modified;
  finally
    ImageListEditorDlg.Free;
  end;
end;


{ TImageListEditorDlgEx }

procedure TImageListEditorDlgEx.AddReplaceImgHandler(Sender: TObject);
var
  replace: Boolean;
begin
  FIconFinderForm := CreateIconFinder;
  try
    if FIconFinderForm.Execute then
    begin
      replace := TAction(Sender).Tag = 1;
      AddReplaceFromIconFinder(FIconFinderForm, replace);
    end;
  finally
    FreeAndNil(FIconFinderForm);
  end;
end;

procedure TImageListEditorDlgEx.AddReplaceFromIconFinder(AIconFinder: TIconFinderForm;
  AReplace: Boolean);
var
  res: TCustomImageListResolution;
  sizes: array of TPoint = nil;
  pictures: array of TPicture = nil;
  i: Integer;
begin
  if ImageList.ResolutionCount = 0 then
  begin
    SetLength(sizes, 1);
    sizes[0] := Point(ImageList.Width, ImageList.Height);
  end else
  begin
    SetLength(sizes, ImageList.ResolutionCount);
    for i := 0 to High(sizes) do
    begin
      res := ImageList.ResolutionByIndex[i];  // they are ordered by size
      sizes[i] := Point(res.Width, res.Height);
    end;
  end;

  try
    SetLength(pictures, Length(sizes));
    for i := 0 to High(pictures) do
      pictures[i] := TPicture.Create;

    // Get pictures form icon finder
    AIconFinder.LoadPictureSizesFromIconFinder(sizes, pictures);

    // First, add the largest image to the imagelist
    if AReplace then
      InternalAddImageToList(pictures[High(pictures)], atReplaceAllResolutions)
    else
      InternalAddImageToList(pictures[High(pictures)], atAdd);

    // Then iterate over all other sizes requested and add them to the imagelist
    for i := Length(pictures)-2 downto 0 do
      InternalAddImageToList(pictures[i], atReplace);
  finally
    for i := 0 to High(pictures) do
      pictures[i].Free;
  end;

  ImageListbox.Invalidate;
  UpdatePreviewImage;
end;

function TImageListEditorDlgEx.CreateIconFinder: TIconFinderForm;
var
  L, T: Integer;
  R: TRect;
begin
  Result := TIconFinderForm.Create(self);
  R := Screen.DesktopRect;
  L := Left + Width;
  if L + Result.Width > R.Right then
  begin
    L := Left - Result.Width;
    if L < R.Left then
      L := Left + (Width - Result.Width) div 2;
  end;
  T := Top;
  Result.Left := L;
  Result.Top := T;
  Result.OnIconDblClick := @IconFinderDblClick;
  Result.ReadSettings('ImageListComponentEditor');
end;

procedure TImageListEditorDlgEx.FormCreate(Sender: TObject);
var
  acAdd: TAction;
  acReplace: TAction;
  mAdd: TMenuItem;
  mReplace: TMenuItem;
begin
  inherited;

  acAdd := TAction.Create(ActionList);
  acAdd.Caption := RSImgListEditor_AddFromIconFinder;
  acAdd.ImageIndex := IDEImages.GetImageIndex('add_icon_from_finder', 16);
  acAdd.OnExecute := @AddReplaceImgHandler;

  acReplace := TAction.Create(ActionList);
  acReplace.Caption := RSImgListEditor_ReplaceFromIconFinder;
  acReplace.ImageIndex := IDEImages.GetImageIndex('replace_by_finder', 16);
  acReplace.OnExecute := @AddReplaceImgHandler;
  acReplace.Tag := 1;  // to distinguish from acAdd having the same handler

  mAdd := TMenuItem.Create(AddPopupMenu);
  mAdd.Action := acAdd;
  AddPopupMenu.Items.Insert(3, mAdd);

  mReplace := TMenuItem.Create(ReplacePopupMenu);
  mReplace.Action := acReplace;
  ReplacePopupMenu.Items.Add(mReplace);
end;

procedure TImageListEditorDlgEx.IconFinderDblClick(Sender: TObject);
begin
  if FIconFinderForm <> nil then
    FIconFinderForm.ModalResult := mrOK;
end;


{ TImageListComponentEditorEx }

procedure TImageListComponentEditorEx.DoShowEditor;
var
  Hook: TPropertyEditorHook;
  AImg: TImageList;
begin
  if GetComponent is TImageList then
  begin
    AImg := TImageList(GetComponent);
    GetHook(Hook);

    if EditImageList(AImg) then
      if Assigned(Hook) then Hook.Modified(Self);
  end;
end;

procedure TImageListComponentEditorEx.ExecuteVerb(Index: Integer);
begin
  DoShowEditor;
end;

function TImageListComponentEditorEx.GetVerb(Index: Integer): String;
begin
  Result := oisImageListComponentEditor;
end;

function TImageListComponentEditorEx.GetVerbCount: Integer;
begin
  Result := 1;
end;


end.

