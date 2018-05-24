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
  LCLProc, Forms, Controls, Graphics, GraphType, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, ExtDlgs, ColorBox, Buttons, ButtonPanel, ImgList, LCLTaskDialog,
  LCLIntf, LCLType,
  // IdeIntf
  IDEDialogs, PropEdits, ComponentEditors, ObjInspStrConsts, IDEWindowIntf, Types;

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
    BtnAdd: TButton;
    BtnClear: TButton;
    BtnDelete: TButton;
    BtnReplace: TButton;
    BtnMoveUp: TButton;
    BtnMoveDown: TButton;
    BtnSave: TButton;
    btnSaveAll: TButton;
    BtnPanel: TButtonPanel;
    ColorBoxTransparent: TColorBox;
    GroupBoxL: TGroupBox;
    GroupBoxR: TGroupBox;
    ImageList: TImageList;
    LabelTransparent: TLabel;
    OpenDialog: TOpenPictureDialog;
    RadioGroup: TRadioGroup;
    Preview: TScrollBox;
    SaveDialog: TSavePictureDialog;
    ImageListBox: TListBox;
    btnAddNewResolution: TButton;
    BtnReplaceAll: TButton;
    BtnAddMoreResolutions: TButton;
    btnDeleteResolution: TButton;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnReplaceClick(Sender: TObject);
    procedure BtnMoveUpClick(Sender: TObject);
    procedure btnSaveAllClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure ColorBoxTransparentClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure btnAddNewResolutionClick(Sender: TObject);
    procedure btnDeleteResolutionClick(Sender: TObject);
    procedure ImageListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure FormResize(Sender: TObject);
  private
    FImageList: TImageList;
    FModified: Boolean;
    FImagesGroupBoxMaxWidth: Integer;
    FPreviewImages: array of TImage;
    FPreviewLabels: array of TLabel;
    procedure SavePicture(Picture: TPicture);
    function GetSelGlyphInfo: TGlyphInfo;
    function GetGlyphInfo(const aItemIndex: Integer): TGlyphInfo;
    procedure RefreshItemHeight;
    procedure FreeGlyphInfos;
    procedure RecreatePreviewImages(const aForce: Boolean = False);
    procedure UpdatePreviewImage;
    procedure UpdateImagesGroupBoxWidth;
    procedure UpdateImagesGroupBoxWidthQueue({%H-}Data: PtrInt);
    class function ResolutionToString(const ARes: TCustomImageListResolution): string;
  protected
    procedure DoDestroy; override;
  public
    procedure LoadFromImageList(AImageList: TImageList);
    procedure SaveToImageList;

    procedure AddImageToList(const FileName: String; AddType: TAddType);
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

procedure TImageListEditorDlg.FormCreate(Sender: TObject);
begin
  Caption := sccsILEdtCaption;

  GroupBoxL.Caption := sccsILEdtGrpLCaption;
  GroupBoxR.Caption := sccsILEdtGrpRCaption;

  BtnAdd.Caption := sccsILEdtAdd;
  BtnAddMoreResolutions.Caption := sccsILEdtAddMoreResolutions;
  BtnDelete.Caption := sccsILEdtDelete;
  BtnReplace.Caption := sccsILEdtReplace;
  BtnReplaceAll.Caption := sccsILEdtReplaceAllResolutions;
  BtnClear.Caption := sccsILEdtClear;
  BtnMoveUp.Caption := sccsILEdtMoveUp;
  BtnMoveDown.Caption := sccsILEdtMoveDown;
  BtnSave.Caption := sccsILEdtSave;
  BtnSaveAll.Caption := sccsILEdtSaveAll;
  BtnAddNewResolution.Caption := sccsILEdtAddNewResolution;
  btnDeleteResolution.Caption := sccsILEdtDeleteResolution;

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

procedure TImageListEditorDlg.FormResize(Sender: TObject);
begin
  UpdateImagesGroupBoxWidth;
end;

procedure TImageListEditorDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
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
  X, Y, NewImagesGroupBoxMaxWidth: Integer;
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
  NewImagesGroupBoxMaxWidth := X + GetSystemMetrics(SM_CXVSCROLL) + GetSystemMetrics(SM_SWSCROLLBARSPACING) + Control.Scale96ToFont(6);
  if FImagesGroupBoxMaxWidth<>NewImagesGroupBoxMaxWidth then
    Application.QueueAsyncCall(@UpdateImagesGroupBoxWidthQueue, 0);
  FImagesGroupBoxMaxWidth := NewImagesGroupBoxMaxWidth;
  C.Clipping := False;
end;

procedure TImageListEditorDlg.ImageListBoxSelectionChange(Sender: TObject;
  User: boolean);
begin
  UpdatePreviewImage;
end;

procedure TImageListEditorDlg.BtnAddClick(Sender: TObject);
var
  I: Integer;
begin
  OpenDialog.Title := sccsILEdtOpenDialog;
  OpenDialog.Options:=OpenDialog.Options+[ofAllowMultiSelect];
  if OpenDialog.Execute then
  begin
    ImageList.BeginUpdate;
    ImageListBox.Items.BeginUpdate;
    try
      for I := 0 to OpenDialog.Files.Count - 1 do
      begin
        if (I = 0) or (Sender<>BtnAddMoreResolutions) then
          AddImageToList(TrimRight(OpenDialog.Files[I]), atAdd)
        else
          AddImageToList(TrimRight(OpenDialog.Files[I]), atReplace);
      end;
    finally
      ImageListBox.Items.EndUpdate;
      ImageList.EndUpdate;
    end;
    ImageListBox.SetFocus;
  end;
end;

procedure TImageListEditorDlg.btnDeleteResolutionClick(Sender: TObject);
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
    UpdateImagesGroupBoxWidth;
  end;
end;

procedure TImageListEditorDlg.BtnClearClick(Sender: TObject);
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

procedure TImageListEditorDlg.BtnDeleteClick(Sender: TObject);
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

procedure TImageListEditorDlg.BtnReplaceClick(Sender: TObject);
var
  AT: TAddType;
begin
  if ImageListBox.ItemIndex>=0 then
  begin
    OpenDialog.Title := sccsILEdtOpenDialogN;
    OpenDialog.Options:=OpenDialog.Options-[ofAllowMultiSelect];
    if OpenDialog.Execute then
    begin
      if Sender=BtnReplaceAll then
        AT := atReplaceAllResolutions
      else
        AT := atReplace;
      AddImageToList(TrimRight(OpenDialog.FileName), AT);
      ImageListBox.SetFocus;
    end;
  end;
end;

procedure TImageListEditorDlg.btnAddNewResolutionClick(Sender: TObject);
var
  R: Longint;
  Res: TDragImageListResolution;
begin
  if TryStrToInt(InputBox(sccsILEdtAddNewResolution, sccsILEdtImageWidthOfNewResolution, ''), R) then
  begin
    Res := ImageList.Resolution[R];
    Res.AutoCreatedInDesignTime := False;
    RefreshItemHeight;
    ImageListBox.Repaint;
    UpdateImagesGroupBoxWidth;
  end;
end;

procedure TImageListEditorDlg.BtnMoveUpClick(Sender: TObject);
var
  S, D: Integer;
  P: TObject;
begin
  if ImageListBox.ItemIndex > 0 then
  begin
    S := ImageListBox.ItemIndex;
    D := (Sender as TControl).Tag;
    if (S + D >= 0) and (S + D < ImageListBox.Items.Count) then
    begin
      ImageList.Move(S, S + D);
      
      P := ImageListBox.Items.Objects[S + D];
      ImageListBox.Items.Objects[S + D] := ImageListBox.Items.Objects[S];
      ImageListBox.Items.Objects[S] := P;
      
      ImageListBox.ItemIndex := S + D;
      ImageListBox.SetFocus;
    end;
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

procedure TImageListEditorDlg.btnSaveAllClick(Sender: TObject);
var
  Picture: TPicture;
begin
  if (ImageList.Count > 0) then
  begin
    Picture := TPicture.Create;
    try
      ImageList.GetFullBitmap(Picture.Bitmap);
      SavePicture(Picture);
    finally
      Picture.Free;
    end;
  end;
end;

procedure TImageListEditorDlg.BtnSaveClick(Sender: TObject);
var
  Picture: TPicture;
begin
  if ImageListBox.ItemIndex>=0 then
  begin
    Picture := TPicture.Create;
    try
      ImageList.GetBitmap(ImageListBox.ItemIndex, Picture.Bitmap);
      SavePicture(Picture);
    finally
      Picture.Free;
    end;
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
  MaxHeight := Scale96ToFont(32);
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

procedure TImageListEditorDlg.btnApplyClick(Sender: TObject);
begin
  SaveToImageList;
end;

procedure TImageListEditorDlg.FormShow(Sender: TObject);
begin
  AlignButtons([
    BtnAdd,
    BtnAddMoreResolutions,
    BtnReplace,
    BtnReplaceAll,
    BtnDelete,
    BtnClear,
    BtnMoveUp,
    BtnMoveDown,
    BtnSave,
    btnSaveAll,
    btnAddNewResolution,
    btnDeleteResolution]);
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
      UpdatePreviewImage;
      UpdateImagesGroupBoxWidth;
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

procedure TImageListEditorDlg.UpdateImagesGroupBoxWidth;
var
  NewWidth: Integer;
begin
  NewWidth := (ClientWidth + BtnAdd.Width) div 2;
  if (FImagesGroupBoxMaxWidth>0) then
    NewWidth := Min(NewWidth, FImagesGroupBoxMaxWidth + BtnAdd.Width + 5*BtnAdd.BorderSpacing.Right);
  GroupBoxL.Width := NewWidth;
  GroupBoxR.Left := GroupBoxL.BoundsRect.Right + Scale96ToFont(4);
end;

procedure TImageListEditorDlg.UpdateImagesGroupBoxWidthQueue(Data: PtrInt);
begin
  UpdateImagesGroupBoxWidth;
end;

procedure TImageListEditorDlg.AddImageToList(const FileName: String;
  AddType: TAddType);
var
  SrcBmp, DestBmp: TBitmap;
  Picture: TPicture;
  P: TGlyphInfo;
begin
  SaveDialog.InitialDir := ExtractFileDir(FileName);
  SrcBmp := nil;
  
  ImageList.BeginUpdate;
  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(FileName);
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
