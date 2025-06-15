{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

@author(Olivier guilbaud (OG) <golivier@free.fr>), Tomas Gregorovic
@created(24/02/2003)
@lastmod(25/02/2003)

Property editor for TImageList objects

History
 26-Feb-2003 OG - Update for use assign.
 27-feb-2003 OG - If possible zoom x2 the selected image.
                - Fix the superposition of images
 27-Jan-2006 TG - Form converted to lfm.
 
Todo :
  - masks and bitmap transparency
}

unit ImageListEditor;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  // LCL
  LCLIntf, LCLType, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ExtDlgs, ColorBox, Buttons, ButtonPanel, ImgList, LCLTaskDialog, ComCtrls,
  Menus, ActnList,
  // LazUtils
  GraphType, LazLoggerBase,
  // IdeIntf
  IDEDialogs, PropEdits, ComponentEditors, ObjInspStrConsts, IDEWindowIntf,
  IDEImagesIntf;

type
  TGlyphAdjustment = (gaNone, gaStretch, gaCrop, gaCenter);

  TGlyphInfo = class
  public
    Bitmap: TBitmap;
    Adjustment: TGlyphAdjustment;
    TransparentColor: TColor;
  public
    destructor Destroy; override;
  end;
  
  { TImageListEditorDlg }

  TAddType = (atAdd, atInsert, atReplace, atReplaceAllResolutions);

  TImageListEditorDlg = class(TForm)
    acAddSingle: TAction;
    acAddMultiple: TAction;
    acAddSliced: TAction;
    acPasteFromClipboard: TAction;
    acReplaceSingle: TAction;
    acReplaceAll: TAction;
    acDelete: TAction;
    acClear: TAction;
    acMoveUp: TAction;
    acMoveDown: TAction;
    acSave: TAction;
    acSaveAll: TAction;
    acNewResolution: TAction;
    acDeleteResolution: TAction;
    ActionList: TActionList;
    BtnPanel: TButtonPanel;
    ColorBoxTransparent: TColorBox;
    GroupBoxL: TGroupBox;
    GroupBoxR: TGroupBox;
    ImageList: TImageList;
    LabelTransparent: TLabel;
    mAddMultiple: TMenuItem;
    mAddSliced: TMenuItem;
    mDelete: TMenuItem;
    mClear: TMenuItem;
    mDeleteSize: TMenuItem;
    mNewSize: TMenuItem;
    mSaveAll: TMenuItem;
    mSave: TMenuItem;
    mPasteFromClipboard: TMenuItem;
    mReplaceAll: TMenuItem;
    mReplaceSingle: TMenuItem;
    miAddSingle: TMenuItem;
    OpenDialog: TOpenPictureDialog;
    AddPopupMenu: TPopupMenu;
    SizesPopupMenu: TPopupMenu;
    SavePopupMenu: TPopupMenu;
    RemovePopupMenu: TPopupMenu;
    ReplacePopupMenu: TPopupMenu;
    RadioGroup: TRadioGroup;
    Preview: TScrollBox;
    SaveDialog: TSavePictureDialog;
    ImageListBox: TListBox;
    Separator1: TMenuItem;
    Splitter1: TSplitter;
    ToolBar: TToolBar;
    tbAdd: TToolButton;
    tbReplace: TToolButton;
    tbMoveUp: TToolButton;
    tbMoveDown: TToolButton;
    tbRemove: TToolButton;
    tbSave: TToolButton;
    tbSeparator1: TToolButton;
    tbSeparator2: TToolButton;
    tbSeparator3: TToolButton;
    tbResolutions: TToolButton;
    procedure acAddExecute(Sender: TObject);
    procedure acAddSlicedExecute(Sender: TObject);
    procedure acClearExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDeleteResolutionExecute(Sender: TObject);
    procedure acMoveUpDownExecute(Sender: TObject);
    procedure acNewResolutionExecute(Sender: TObject);
    procedure acPasteFromClipboardExecute(Sender: TObject);
    procedure acReplaceAllExecute(Sender: TObject);
    procedure acReplaceSingleExecute(Sender: TObject);
    procedure acSaveOneOrAllExecute(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure ColorBoxTransparentClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ImageListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure ImageListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
  private
    FImageList: TImageList;
    FModified: Boolean;
//    FImagesGroupBoxMaxWidth: Integer;
    FPreviewImages: array of TImage;
    FPreviewLabels: array of TLabel;
    procedure SavePicture(Picture: TPicture);
    function GetSelGlyphInfo: TGlyphInfo;
    function GetGlyphInfo(const aItemIndex: Integer): TGlyphInfo;
    procedure RefreshItemHeight;
    procedure FreeGlyphInfos;
    procedure RecreatePreviewImages(const aForce: Boolean = False);
//    procedure UpdateImagesGroupBoxWidth;
//    procedure UpdateImagesGroupBoxWidthQueue({%H-}Data: PtrInt);
    class function ResolutionToString(const ARes: TCustomImageListResolution): string;
    procedure PasteFromClipboardAndAdd;
  protected
    procedure DoDestroy; override;
    procedure InternalAddImageToList(const Picture: TPicture; AddType: TAddType);
    procedure UpdateCmds; virtual;
    procedure UpdateMenus; virtual;
    procedure UpdatePreviewImage; virtual;
  public
    procedure LoadFromImageList(AImageList: TImageList);
    procedure SaveToImageList;
    procedure AddImageToList(const FileName: String; AddType: TAddType);
    procedure AddSlicedImagesToList(const FileName: String);
    property Modified: Boolean read FModified write FModified;
  end;

  //Editor call by Lazarus with 1 verbe only

  { TImageListComponentEditor }

  TImageListComponentEditor = class(TComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb({%H-}Index: Integer); override;
    function GetVerb({%H-}Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;


implementation

{$R *.lfm}

uses
  ClipBrd;

procedure AlignButtons(AButtons: array of TControl);
var
  Button: TControl;
  MaxWidth: Integer;
begin
  // In designer:
  // Left sides of buttons anchored to TListBox
  // Right sides of buttons are not anchored
  // Buttons, GroupBox: AutoSize=True
  MaxWidth:=0;
  for Button in AButtons do
    if Button.Width > MaxWidth then
      MaxWidth:=Button.Width;
  for Button in AButtons do
    Button.Constraints.MinWidth:=MaxWidth;
end;

function EditImageList(AImageList: TImageList): Boolean;
var
  ImageListEditorDlg: TImageListEditorDlg;
begin
  ImageListEditorDlg := TImageListEditorDlg.Create(Application);
  try
    ImageListEditorDlg.LoadFromImageList(AImageList);

    if ImageListEditorDlg.ShowModal = mrOk then
      ImageListEditorDlg.SaveToImageList;

    Result := ImageListEditorDlg.FModified;
  finally
    ImageListEditorDlg.Free;
  end;
end;

function CreateGlyph(B: TBitmap; Width, Height: Integer;
  Adjustment: TGlyphAdjustment; TransparentColor: TColor = clDefault): TBitmap;
begin
  Result := TBitmap.Create;
  if (Adjustment = gaNone) then
  begin
    Result.Assign(B);
  end
  else
  begin
    Result.Width := Width;
    Result.Height := Height;
    Result.Canvas.Brush.Color := TransparentColor;
    Result.Canvas.FillRect(Bounds(0, 0, Width, Height));

    case Adjustment of
      gaStretch: Result.Canvas.StretchDraw(Bounds(0, 0, Width, Height), B);
      gaCrop: Result.Canvas.Draw(0, 0, B);
      gaCenter: Result.Canvas.Draw((Width - B.Width) div 2, (Height - B.Height) div 2, B);
    end;
  end;
  if TransparentColor = clDefault then
    Result.TransparentMode := tmAuto
  else
  begin
    Result.TransparentColor := TransparentColor;
    Result.TransparentMode := tmFixed;
  end;
  Result.Transparent := True;
end;

function CreateGlyphSplit(Src: TBitmap; Width, Height: Integer;
  RowIndex, ColIndex: Integer): TBitmap;
var
  SrcRect: TRect;
  SrcRawImage, DstRawImage: TRawImage;
begin
  // Ensure that the returned Bitmap is not bigger than Src
  Width := Min(Width, Src.Width);
  Height := Min(Height, Src.Height);
  SrcRect := Bounds(ColIndex * Width, RowIndex * Height, Width, Height);
  // copy raw image, instead of using Canvas functions to preserve transparency
  SrcRawImage := Src.RawImage;
  DstRawImage.Init;
  DstRawImage.Description := SrcRawImage.Description;
  DstRawImage.Description.Width := Width;
  DstRawImage.Description.Height := Height;
  SrcRawImage.ExtractRect(SrcRect, DstRawImage);
  Result := TBitmap.Create;
  Result.TransparentColor := Src.TransparentColor;
  Result.TransparentMode := Src.TransparentMode;
  Result.Transparent := True;
  Result.LoadFromRawImage(DstRawImage, True);
end;

{ TGlyphInfo }

destructor TGlyphInfo.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

{ TImageListEditorDlg }

procedure TImageListEditorDlg.FormActivate(Sender: TObject);
begin
  UpdateMenus;
end;

procedure TImageListEditorDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TImageListEditorDlg.FormCreate(Sender: TObject);
begin
  Caption := sccsILEdtCaption;

  ActionList.Images := IDEImages.Images_16;
  ToolBar.Images := IDEImages.Images_16;
  AddPopupMenu.Images := IDEImages.Images_16;
  ReplacePopupMenu.Images := IDEImages.Images_16;
  RemovePopupMenu.Images := IDEImages.Images_16;
  SavePopupMenu.Images := IDEImages.Images_16;
  SizesPopupMenu.Images := IDEImages.Images_16;

  tbAdd.Caption := sccsILEdtAdd;
  tbReplace.Caption := sccsILEdtReplace;
  tbRemove.Caption := sccsILEdtRemove;
  tbSave.Caption := sccsILEdtSaveBtn;
  tbResolutions.Caption := sccsILEdtResolutionsBtn;
  acAddSingle.Caption := sccsILEdtAddSingleResolution;
  acAddMultiple.Caption := sccsILEdtAddMoreResolutions;
  acAddSliced.Caption := sccsILEdtAddSliced;
  acPasteFromClipboard.Caption := sccsILEdtPasteFromClipboard;
  acReplaceSingle.Caption := sccsILEdtReplaceSingleResolution;
  acReplaceAll.Caption := sccsILEdtReplaceAllResolutions;
  acDelete.Caption := sccsILEdtDelete;
  acClear.Caption := sccsILEdtClear;
  acMoveUp.Caption := sccsILEdtMoveUp;
  acMoveDown.Caption := sccsILEdtMoveDown;
  acSave.Caption := sccsILEdtSave;
  acSaveAll.Caption := sccsILEdtSaveAll;
  acNewResolution.Caption := sccsILEdtAddNewResolution;
  acDeleteResolution.Caption := sccsILEdtDeleteResolution;

  tbAdd.ImageIndex := IDEImages.GetImageIndex('laz_add', 16);
    acAddSingle.ImageIndex := IDEImages.GetImageIndex('add_icon_single', 16);
    acAddMultiple.ImageIndex := IDEImages.GetImageIndex('add_icon_multiple', 16);
    acPasteFromClipboard.ImageIndex := IDEImages.GetImageIndex('add_icon_from_clipboard', 16);
  tbReplace.ImageIndex := IDEImages.GetImageIndex('laz_refresh', 16);
    acReplaceSingle.ImageIndex := IDEImages.GetImageIndex('replace_icon_single', 16);
    acReplaceAll.ImageIndex := IDEImages.GetImageIndex('replace_icon_multiple', 16);
  tbRemove.ImageIndex := IDEImages.GetImageIndex('laz_delete', 16);
    acDelete.ImageIndex := IDEImages.GetImageIndex('remove_icon_single', 16);
    acClear.ImageIndex := IDEImages.GetImageIndex('remove_icon_multiple', 16);
  acMoveUp.ImageIndex := IDEImages.GetImageIndex('arrow_up', 16);
  acMoveDown.ImageIndex := IDEImages.GetImageIndex('arrow_down', 16);
  tbSave.ImageIndex := IDEImages.GetImageIndex('laz_save', 16);
    acSave.ImageIndex := IDEImages.GetImageIndex('menu_saveas', 16);
    acSaveAll.ImageIndex := IDEImages.GetImageIndex('menu_save_all', 16);
  tbResolutions.ImageIndex := IDEImages.GetImageIndex('resolution', 16);
    acNewResolution.ImageIndex := IDEImages.GetImageIndex('resolution_add', 16);
    acDeleteResolution.ImageIndex := IDEImages.GetImageIndex('resolution_remove', 16);

  GroupBoxL.Caption := sccsILEdtGrpLCaption;
  GroupBoxR.Caption := sccsILEdtGrpRCaption;

  BtnPanel.HelpButton.Caption := oisHelp;
  BtnPanel.OKButton.Caption := oisOK;
  BtnPanel.CancelButton.Caption := oisCancel;

  BtnPanel.CloseButton.Caption := sccsILEdtApply;
  BtnPanel.CloseButton.Kind := bkCustom;
  BtnPanel.CloseButton.Glyph := nil;
  BtnPanel.CloseButton.ModalResult := mrNone;
  BtnPanel.CloseButton.OnClick := @btnApplyClick;

  LabelTransparent.Caption := sccsILEdtransparentColor;

  RadioGroup.Caption := sccsILEdtAdjustment;
  RadioGroup.Items[0] := sccsILEdtNone;
  RadioGroup.Items[1] := sccsILEdtStretch;
  RadioGroup.Items[2] := sccsILEdtCrop;
  RadioGroup.Items[3] := sccsILEdtCenter;
  
  OpenDialog.Title := sccsILEdtOpenDialog;
  SaveDialog.Title := sccsILEdtSaveDialog;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TImageListEditorDlg.FreeGlyphInfos;
var
  I: Integer;
begin
  for I := 0 to ImageListBox.Items.Count-1 do
    ImageListBox.Items.Objects[I].Free;
end;

function TImageListEditorDlg.GetGlyphInfo(
  const aItemIndex: Integer): TGlyphInfo;
begin
  if (aItemIndex<0) or (aItemIndex>=ImageListBox.Count) then
    Exit(nil);
  Result := TGlyphInfo(ImageListBox.Items.Objects[aItemIndex]);
end;

function TImageListEditorDlg.GetSelGlyphInfo: TGlyphInfo;
begin
  Result := GetGlyphInfo(ImageListBox.ItemIndex);
end;

procedure TImageListEditorDlg.ImageListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  C: TCanvas;
  X, Y{, NewImagesGroupBoxMaxWidth}: Integer;
  R: TCustomImageListResolution;
begin
  C := (Control as TListBox).Canvas;
  C.FillRect(ARect);

  X := ARect.Left+Control.Scale96ToFont(2);
  Y := ARect.Top+Control.Scale96ToFont(2);
  C.TextOut(X, Y, IntToStr(Index));
  Inc(X, C.TextWidth('0')*4);
  C.ClipRect := ARect;
  C.Clipping := True;

  for R in ImageList.Resolutions do
  begin
    if X<ARect.Right then
      R.Draw(C, X, Y, Index);
    Inc(X, R.Width+Control.Scale96ToFont(5));
  end;
  (*
  NewImagesGroupBoxMaxWidth := X + GetSystemMetrics(SM_CXVSCROLL) + GetSystemMetrics(SM_SWSCROLLBARSPACING) + Control.Scale96ToFont(6);
  if FImagesGroupBoxMaxWidth<>NewImagesGroupBoxMaxWidth then
    Application.QueueAsyncCall(@UpdateImagesGroupBoxWidthQueue, 0);
  FImagesGroupBoxMaxWidth := NewImagesGroupBoxMaxWidth;
  *)
  C.Clipping := False;
end;

procedure TImageListEditorDlg.ImageListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  UpdatePreviewImage;
  UpdateCmds;
end;

procedure TImageListEditorDlg.acAddExecute(Sender: TObject);
var
  I: Integer;
begin
  OpenDialog.Title := sccsILEdtOpenDialog;
  OpenDialog.Options := OpenDialog.Options+[ofAllowMultiSelect];
  if OpenDialog.Execute then
  begin
    ImageList.BeginUpdate;
    ImageListBox.Items.BeginUpdate;
    try
      for I := 0 to OpenDialog.Files.Count - 1 do
      begin
        if (I = 0) or (Sender <> acAddMultiple) then
          AddImageToList(TrimRight(OpenDialog.Files[I]), atAdd)
        else
          AddImageToList(TrimRight(OpenDialog.Files[I]), atReplace);
      end;
    finally
      ImageListBox.Items.EndUpdate;
      ImageList.EndUpdate;
    end;
    UpdatePreviewImage;
    ImageListBox.SetFocus;
  end;
end;

procedure TImageListEditorDlg.acAddSlicedExecute(Sender: TObject);
begin
  OpenDialog.Title := sccsILEdtOpenDialog;
  if OpenDialog.Execute then
  begin
    ImageList.BeginUpdate;
    ImageListBox.Items.BeginUpdate;
    try
      AddSlicedImagesToList(OpenDialog.Filename);
    finally
      ImageListBox.Items.EndUpdate;
      ImageList.EndUpdate;
    end;
    UpdatePreviewImage;
    ImageListBox.SetFocus;
  end;
end;

procedure TImageListEditorDlg.acClearExecute(Sender: TObject);
begin
  if ImageListBox.Items.Count=0 then exit;
  if (IDEQuestionDialog(Caption,
              s_Confirm_Clear, mtConfirmation,
              [mrYes, mrNo]) = mrYes) then
  begin
    FreeGlyphInfos;
    ImageList.Clear;
    ImageListBox.Items.Clear;
  end;
end;

procedure TImageListEditorDlg.acDeleteExecute(Sender: TObject);
var
  S: Integer;
begin
  if ImageListBox.ItemIndex>=0 then
  begin
    S := ImageListBox.ItemIndex;

    ImageList.Delete(S);
    ImageListBox.Items.Objects[S].Free;
    ImageListBox.Items.Delete(S);
    ImageListBox.ItemIndex := Min(S, ImageListBox.Count-1);
  end;
  ImageListBox.SetFocus;
end;

procedure TImageListEditorDlg.acDeleteResolutionExecute(Sender: TObject);
var
  TD: LCLTaskDialog.TTaskDialog;
  R: TCustomImageListResolution;
  RA: array of Integer;
  ResItem: string;
begin
  FillChar(TD{%H-}, SizeOf(LCLTaskDialog.TTaskDialog), 0);
  SetLength(RA, 0);
  for R in ImageList.Resolutions do
  begin
    if R.Width=ImageList.Width then // cannot delete default resolution
      continue;

    if TD.Selection<>'' then
      TD.Selection += sLineBreak;
    ResItem := ResolutionToString(R);
    TD.Selection += ResItem;
    if TD.Query='' then
      TD.Query := ResItem;
    SetLength(RA, Length(RA)+1);
    RA[High(RA)] := R.Width;
  end;

  if TD.Selection='' then
  begin
    MessageDlg(sccsILEdtCannotDeleteResolution, mtError, [mbOK], 0);
    Exit;
  end;

  TD.Inst := sccsILEdtDeleteResolutionConfirmation;
  if TD.Execute([cbOK, cbCancel]) = mrOK then
  begin
    ImageList.DeleteResolution(RA[TD.SelectionRes]);
    ImageListBox.Repaint;
    UpdatePreviewImage;
    UpdateMenus;
  end;
end;

procedure TImageListEditorDlg.acMoveUpDownExecute(Sender: TObject);
var
  OldIndex, NewIndex: Integer;
  P: TObject;
begin
  if ImageListBox.ItemIndex < 0 then
    Exit;

  OldIndex := ImageListBox.ItemIndex;
  if Sender = acMoveUp then
    NewIndex := OldIndex - 1
  else if Sender = acMoveDown then
    NewIndex := OldIndex + 1
  else
    raise Exception.Create('[acMoveUpdownExecute] Called by unknown action.');

  if (NewIndex >= 0) and (NewIndex < ImageListBox.Items.Count) then
  begin
    ImageList.Move(OldIndex, NewIndex);

    P := ImageListBox.Items.Objects[NewIndex];
    ImageListBox.Items.Objects[NewIndex] := ImageListBox.Items.Objects[OldIndex];
    ImageListBox.Items.Objects[OldIndex] := P;

    ImageListBox.ItemIndex := NewIndex;
    ImageListBox.SetFocus;

    UpdateCmds;
  end;
end;

procedure TImageListEditorDlg.acNewResolutionExecute(Sender: TObject);
var
  R: Longint;
  Res: TDragImageListResolution;
  px, perc: Integer;
  NewWidthStr: String;
begin
  perc := cInputQueryEditSizePercents;
  px := cInputQueryEditSizePixels;
  cInputQueryEditSizePercents := 0;
  cInputQueryEditSizePixels := 250;

  NewWidthStr := InputBox(sccsILEdtAddNewResolution, sccsILEdtImageWidthOfNewResolution, '');
  if TryStrToInt(NewWidthStr, R) and (R > 0) then
  begin
    Res := ImageList.Resolution[R];
    Res.AutoCreatedInDesignTime := False;
    RefreshItemHeight;
    ImageListBox.Repaint;
    UpdateMenus;
  end else
    MessageDlg(Format(sccsILEdtNoValidImageWidth, [NewWidthStr]), mtError, [mbOK], 0);

  cInputQueryEditSizePercents := perc;
  cInputQueryEditSizePixels := px;
end;

procedure TImageListEditorDlg.acPasteFromClipboardExecute(Sender: TObject);
begin
  PasteFromClipboardAndAdd;
end;

procedure TImageListEditorDlg.acReplaceAllExecute(Sender: TObject);
var
  i: Integer;
  w: Integer = 0;
  bigIdx: Integer = 0;
  pic: TPicture;
begin
  if ImageListBox.ItemIndex >= 0 then
  begin
    OpenDialog.Title := sccsILEdtOpenDialogN;
    if Sender = acReplaceAll then
      OpenDialog.Options := OpenDialog.Options + [ofAllowMultiSelect]
    else
      OpenDialog.Options := OpenDialog.Options - [ofAllowMultiSelect];

    if OpenDialog.Execute then
    begin
      try
        ImageList.BeginUpdate;
        ImageListBox.Items.BeginUpdate;
        // Replace all icons
        if Sender = acReplaceAll then
        begin
          // Find the largest icon
          pic := TPicture.Create;
          try
            for i := 0 to OpenDialog.Files.Count - 1 do
            begin
              pic.LoadFromFile(TrimRight(OpenDialog.Files[i]));
              if pic.Width > W then
              begin
                w := pic.Width;
                bigIdx := i;
              end;
            end;
          finally
            pic.Free;
          end;
          // Create all icons from the largest icon. This makes sure that
          // also non-selected icons will be replaced.
          AddImageToList(TrimRight(OpenDialog.Files[bigIdx]), atReplaceAllResolutions);
          // Replace other  icons by their file versions when selected.
          for i := 0 to OpenDialog.Files.Count - 1 do
            if i <> bigIdx then
              AddImageToList(TrimRight(OpenDialog.Files[i]), atReplace);
        end
        // Replace only a single icon
        else
          AddImageToList(TrimRight(OpenDialog.FileName), atReplace);
      finally
        ImageListBox.Items.EndUpdate;
        ImageList.EndUpdate;
      end;
      UpdatePreviewImage;
      ImageListBox.SetFocus;
    end;
  end;
end;

procedure TImageListEditorDlg.acReplaceSingleExecute(Sender: TObject);
var
  i: Integer;
  w: Integer = 0;
  bigIdx: Integer = 0;
  pic: TPicture;
begin
  if ImageListBox.ItemIndex >= 0 then
  begin
    OpenDialog.Title := sccsILEdtOpenDialogN;
    if Sender = acReplaceAll then
      OpenDialog.Options := OpenDialog.Options + [ofAllowMultiSelect]
    else
      OpenDialog.Options := OpenDialog.Options - [ofAllowMultiSelect];

    if OpenDialog.Execute then
    begin
      try
        ImageList.BeginUpdate;
        ImageListBox.Items.BeginUpdate;
        // Replace all icons
        if Sender = acReplaceAll then
        begin
          // Find the largest icon
          pic := TPicture.Create;
          try
            for i := 0 to OpenDialog.Files.Count - 1 do
            begin
              pic.LoadFromFile(TrimRight(OpenDialog.Files[i]));
              if pic.Width > W then
              begin
                w := pic.Width;
                bigIdx := i;
              end;
            end;
          finally
            pic.Free;
          end;
          // Create all icons from the largest icon. This makes sure that
          // also non-selected icons will be replaced.
          AddImageToList(TrimRight(OpenDialog.Files[bigIdx]), atReplaceAllResolutions);
          // Replace other  icons by their file versions when selected.
          for i := 0 to OpenDialog.Files.Count - 1 do
            if i <> bigIdx then
              AddImageToList(TrimRight(OpenDialog.Files[i]), atReplace);
        end
        // Replace only a single icon
        else
          AddImageToList(TrimRight(OpenDialog.FileName), atReplace);
      finally
        ImageListBox.Items.EndUpdate;
        ImageList.EndUpdate;
      end;
      UpdatePreviewImage;
      ImageListBox.SetFocus;
    end;
  end;
end;

procedure TImageListEditorDlg.acSaveOneOrAllExecute(Sender: TObject);
var
  Picture: TPicture;
begin
  if (Sender = acSaveAll) and (ImageList.Count = 0) then
    exit;
  if (Sender = acSave) and (ImageListbox.ItemIndex < 0) then
    exit;

  Picture := TPicture.Create;
  try
    if Sender = acSave then
      ImageList.GetBitmap(ImageListBox.ItemIndex, Picture.Bitmap)
    else if Sender = acSaveAll then
      ImageList.GetFullBitmap(Picture.Bitmap);

    SavePicture(Picture);
  finally
    Picture.Free;
  end;
end;

procedure TImageListEditorDlg.SavePicture(Picture: TPicture);
var
  FileName, Ext: String;
begin
  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
    if ExtractFileExt(FileName) = '' then
    begin
      Ext := SaveDialog.GetFilterExt;
      if Ext = '' then
        Ext := 'bmp';
      if Ext <> '' then
        FileName := FileName + '.' + Ext;
    end;
    Picture.SaveToFile(FileName);
  end;
end;

procedure TImageListEditorDlg.ColorBoxTransparentClick(Sender: TObject);
var
  P: TGlyphInfo;
  T: TBitmap;
begin
  P := GetSelGlyphInfo;
  if Assigned(P) then
  begin
    P.Adjustment := TGlyphAdjustment(RadioGroup.ItemIndex);
    P.TransparentColor := ColorBoxTransparent.Selected;

    T := CreateGlyph(P.Bitmap, ImageList.Width, ImageList.Height, P.Adjustment,
      P.TransparentColor);
    ImageList.BeginUpdate;
    try
      ImageList.Delete(ImageListBox.ItemIndex);
      ImageList.Insert(ImageListBox.ItemIndex, T, nil);
    finally
      ImageList.EndUpdate;
      T.Free;
    end;

    ImageListBox.Invalidate;

  end
end;

procedure TImageListEditorDlg.DoDestroy;
begin
  FreeGlyphInfos;
  inherited DoDestroy;
end;

procedure TImageListEditorDlg.RefreshItemHeight;
var
  R: TCustomImageListResolution;
  ItemHeight, MaxHeight: Integer;
begin
  ItemHeight := ImageListBox.Canvas.TextHeight('Hg');
  if ImageList.ResolutionCount > 0 then  // smallest image will not be truncated
    ItemHeight := Max(ItemHeight, ImageList.ResolutionByIndex[0].Height);
  MaxHeight := Scale96ToFont(32);   // max height above with next sizes will be truncated
  for R in ImageList.Resolutions do
  begin
    if R.Height <= MaxHeight then
      ItemHeight := Max(ItemHeight, R.Height)
    else
      break;
  end;

  ImageListBox.ItemHeight := ItemHeight + Scale96ToFont(4);
end;

class function TImageListEditorDlg.ResolutionToString(
  const ARes: TCustomImageListResolution): string;
begin
  Result := Format('%d x %d', [ARes.Width, ARes.Height]);
end;

procedure TImageListEditorDlg.PasteFromClipboardAndAdd;
var
  Picture: TPicture;
  cf: TClipboardFormat;
begin
  cf := Clipboard.FindPictureFormatID;
  if cf <> 0 then
  begin
    Picture := TPicture.Create;
    try
      Picture.LoadFromClipboardFormat(cf);
      InternalAddImageToList(Picture, atAdd);
    finally
      Picture.Free;
    end;
  end;
end;

procedure TImageListEditorDlg.btnApplyClick(Sender: TObject);
begin
  SaveToImageList;
end;

procedure TImageListEditorDlg.UpdateCmds;
begin
  tbReplace.Enabled := (ImageListBox.ItemIndex >= 0);
  acReplaceSingle.Enabled := tbReplace.Enabled;
  acReplaceAll.Enabled := tbReplace.Enabled;

  tbRemove.Enabled := tbReplace.Enabled;
  acDelete.Enabled := tbRemove.Enabled;
  acClear.Enabled := (ImageList.Count > 0);

  acMoveUp.Enabled := ImageListbox.ItemIndex > 0;
  acMoveDown.Enabled := ImageListbox.ItemIndex < ImageListBox.Count-1;

  acSave.Enabled := tbReplace.Enabled;
  acSaveAll.Enabled := ImageListbox.Items.Count > 0;
  tbSave.Enabled := acSave.Enabled or acSaveAll.Enabled;
end;

procedure TImageListEditorDlg.UpdateMenus;
var
  multipleRes: Boolean;
  nRes: Integer;
begin
  nRes := ImageList.ResolutionCount;
  if (ImageList.Count = 0) then inc(nRes);  // In this case ResolutionCount is not correct.
  multipleRes := nRes > 1;
  acAddMultiple.Visible := multipleRes;
  acReplaceAll.Visible := multipleRes;
  if multipleRes then
  begin
    acAddSingle.Caption := sccsILEdtAddSingleResolution;
    acReplaceSingle.Caption := sccsILEdtReplaceSingleResolution;
  end else
  begin
    acAddSingle.Caption := sccsILEdtAddImg;
    acReplaceSingle.Caption := sccsILEdtReplaceImg;
  end;

end;

procedure TImageListEditorDlg.UpdatePreviewImage;
  procedure DisablePreview;
  begin
    RadioGroup.Enabled := False;
    RadioGroup.OnClick := nil;
    RadioGroup.ItemIndex := 0;
    RadioGroup.OnClick := @ColorBoxTransparentClick;

    ColorBoxTransparent.Enabled := False;
    ColorBoxTransparent.OnChange := nil;
    ColorBoxTransparent.Selected := clFuchsia;
    ColorBoxTransparent.OnChange := @ColorBoxTransparentClick;
  end;
var
  Img: TImage;
  R, I: Integer;
  Res: TCustomImageListResolution;
  P: TGlyphInfo;
begin
  RecreatePreviewImages;
  I := ImageListBox.ItemIndex;
  if I<0 then
  begin
    for R := 0 to High(FPreviewImages) do
      FPreviewImages[R].Picture.Clear;

    DisablePreview;
    Exit;
  end;

  for R := 0 to ImageList.ResolutionCount-1 do
  begin
    Img := FPreviewImages[R];
    Res := ImageList.ResolutionByIndex[R];
    Res.GetBitmap(I, Img.Picture.Bitmap);
  end;

  P := GetSelGlyphInfo;
  if Assigned(P) then
  begin
    RadioGroup.Enabled := True;
    RadioGroup.OnClick := nil;
    RadioGroup.ItemIndex := Integer(P.Adjustment);
    RadioGroup.OnClick := @ColorBoxTransparentClick;

    ColorBoxTransparent.Enabled := True;
    ColorBoxTransparent.OnChange := nil;
    ColorBoxTransparent.Selected := P.TransparentColor;
    ColorBoxTransparent.OnChange := @ColorBoxTransparentClick;
  end else
    DisablePreview;
end;

procedure TImageListEditorDlg.LoadFromImageList(AImageList: TImageList);
var
  I: Integer;
  R: TCustomImageListResolution;
begin
  ImageList.Clear;
  FImageList := AImageList;
  FModified := False;

  if Assigned(AImageList) then
  begin
    ImageList.Assign(AImageList);
    for R in ImageList.ResolutionsDesc do
      if R.AutoCreatedInDesignTime then
        ImageList.DeleteResolution(R.Width);

    ImageListBox.Items.BeginUpdate;
    try
      FreeGlyphInfos;
      ImageListBox.Items.Clear;
      for I := 0 to ImageList.Count-1 do
        ImageListBox.Items.AddObject('', nil);

      RefreshItemHeight;
      if ImageListBox.Items.Count>0 then
        ImageListBox.ItemIndex := 0;
      RecreatePreviewImages(True);
      UpdateCmds;
      UpdatePreviewImage;
//      UpdateImagesGroupBoxWidth;
    finally
      ImageListBox.Items.EndUpdate;
    end;
  end;
end;

procedure TImageListEditorDlg.RecreatePreviewImages(const aForce: Boolean);
var
  I, X, Y: Integer;
  Img: TImage;
  R: TCustomImageListResolution;
  Lbl: TLabel;
begin
  if not aForce and (Length(FPreviewImages)=ImageList.ResolutionCount) then
    Exit;

  for Img in FPreviewImages do
    Img.Free;
  for Lbl in FPreviewLabels do
    Lbl.Free;

  SetLength(FPreviewImages, ImageList.ResolutionCount);
  SetLength(FPreviewLabels, ImageList.ResolutionCount);

  X := Scale96ToFont(4);
  Y := Scale96ToFont(4);

  for I := 0 to ImageList.ResolutionCount-1 do
  begin
    R := ImageList.ResolutionByIndex[I];
    Img := TImage.Create(Self);
    FPreviewImages[I] := Img;
    Lbl := TLabel.Create(Self);
    FPreviewLabels[I] := Lbl;

    Img.Parent := Preview;
    Img.SetBounds(X, Y, R.Width, R.Height);
    Img.Stretch := False;

    Lbl.Parent := Preview;
    Lbl.AnchorParallel(akTop, 0, Img);
    Lbl.AnchorToNeighbour(akLeft, Scale96ToFont(6), Img);
    Lbl.Caption := ResolutionToString(R);

    Inc(Y, Img.Height + X);
  end;
end;

procedure TImageListEditorDlg.SaveToImageList;
begin
  FImageList.Assign(ImageList);
  FModified := True;
end;

(*
procedure TImageListEditorDlg.UpdateImagesGroupBoxWidth;
var
  NewWidth: Integer;
begin
//  NewWidth := (ClientWidth + BtnAdd.Width) div 2;
  NewWidth := ClientWidth div 2;
  {
  if (FImagesGroupBoxMaxWidth > 0) then
    NewWidth := Min(NewWidth, FImagesGroupBoxMaxWidth + BtnAdd.Width + 5*BtnAdd.BorderSpacing.Right);
    }
  GroupBoxL.Width := NewWidth;
  GroupBoxR.Left := GroupBoxL.BoundsRect.Right + Scale96ToFont(4);
end;

procedure TImageListEditorDlg.UpdateImagesGroupBoxWidthQueue(Data: PtrInt);
begin
  UpdateImagesGroupBoxWidth;
end;
        *)
procedure TImageListEditorDlg.InternalAddImageToList(const Picture: TPicture;
  AddType: TAddType);
var
  SrcBmp, DestBmp: TBitmap;
  P: TGlyphInfo = nil;
begin
  SrcBmp := nil;

  ImageList.BeginUpdate;
  try
    if Picture.Graphic is TCustomIcon then
    begin
      ImageListBox.Items.Add('');
      case AddType of
        atAdd: ImageList.AddIcon(TCustomIcon(Picture.Graphic));
        atInsert: ImageList.InsertIcon(ImageListBox.ItemIndex+1, TCustomIcon(Picture.Graphic));
        atReplace, atReplaceAllResolutions: ImageList.ReplaceIcon(ImageListBox.ItemIndex, TCustomIcon(Picture.Graphic));
      end;
    end else
    begin
      SrcBmp := TBitmap.Create;
      if (AddType in [atReplace, atReplaceAllResolutions]) and (GetSelGlyphInfo<>nil) then
      begin
        P := GetSelGlyphInfo;
        P.Bitmap.Free;
        P.Bitmap := SrcBmp;
      end else
      begin
        P := TGlyphInfo.Create;
        P.Bitmap := SrcBmp;
      end;
      P.TransparentColor := clDefault;
      P.Adjustment := gaNone;
      if not (AddType in [atReplace, atReplaceAllResolutions]) then
        ImageListBox.Items.AddObject('', P);

      SrcBmp.Assign(Picture.Graphic);
      DestBmp := CreateGlyph(SrcBmp, SrcBmp.Width, SrcBmp.Height, P.Adjustment, P.TransparentColor);
      try
        case AddType of
          atAdd: ImageList.Add(DestBmp, nil);
          atInsert: ImageList.Insert(ImageListBox.ItemIndex+1, DestBmp, nil);
          atReplace, atReplaceAllResolutions: ImageList.Replace(ImageListBox.ItemIndex, DestBmp, nil, AddType=atReplaceAllResolutions);
        end;
      finally
        DestBmp.Free;
      end;
    end;

    case AddType of
      atAdd: ImageListBox.ItemIndex := ImageListBox.Count-1;
      atInsert: ImageListBox.ItemIndex := ImageListBox.ItemIndex+1;
    end;

    UpdateCmds;
  finally
    ImageList.EndUpdate;
  end;
end;

procedure TImageListEditorDlg.AddImageToList(const FileName: String;
  AddType: TAddType);
var
  Picture: TPicture;
begin
  SaveDialog.InitialDir := ExtractFileDir(FileName);
  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(FileName);
    InternalAddImageToList(Picture, AddType);
  finally
    Picture.Free;
  end;
end;

procedure TImageListEditorDlg.AddSlicedImagesToList(const FileName: String);
var
  SrcBmp, DestBmp: TBitmap;
  Picture: TPicture;
  P: TGlyphInfo = nil;
  i, j: Integer;
begin
  SaveDialog.InitialDir := ExtractFileDir(FileName);
  SrcBmp := nil;

  ImageList.BeginUpdate;
  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(FileName);
    if Picture.Graphic is TCustomIcon then begin
      MessageDlg(sccsILEdtAddSlicedIconError, mtError, [mbOK], 0);
      exit;
    end;

    SrcBmp := TBitmap.Create;
    SrcBmp.Assign(Picture.Graphic);
    DestBmp := CreateGlyph(SrcBmp, SrcBmp.Width, SrcBmp.Height, gaNone, clDefault);
    try
      if (DestBmp.Width mod ImageList.Width = 0) and (DestBmp.Height mod ImageList.Height = 0) then
      begin
        j := ImageList.AddSliced(DestBmp, DestBmp.Width div ImageList.Width, DestBmp.Height div ImageList.Height);
        for i:=j to ImageList.Count - 1 do begin
          P := TGlyphInfo.Create;
          ImageList.GetBitmap(i, P.Bitmap);
          P.TransparentColor := clDefault;
          P.Adjustment := gaNone;
          ImageListbox.Items.AddObject('', P);
        end;
        ImageListbox.ItemIndex := ImageListbox.Count - 1;
      end else
        MessageDlg(sccsILEdtCannotSlice, mtError, [mbOK], 0);
    finally
      DestBmp.Free;
      SrcBmp.Free;
    end;
  finally
    Picture.Free;
    ImageList.EndUpdate;
  end;
end;


{ TImageListComponentEditor }

procedure TImageListComponentEditor.DoShowEditor;
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
  DebugLn('TImageListComponentEditor.DoShowEditor END ');
end;

procedure TImageListComponentEditor.ExecuteVerb(Index: Integer);
begin
  DoShowEditor;
end;

function TImageListComponentEditor.GetVerb(Index: Integer): String;
begin
  Result := oisImageListComponentEditor;
end;

function TImageListComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

initialization
  //Register a component editor for TImageList
  RegisterComponentEditor(TImageList,TImageListComponentEditor);
end.
