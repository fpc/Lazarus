{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, Menus, LCLProc, ColorBox, ExtDlgs,
  IDEDialogs, PropEdits, ComponentEditors, ObjInspStrConsts, ButtonPanel;

type
  TGlyphAdjustment = (gaNone, gaStretch, gaCrop, gaCenter);

  PGlyphInfo = ^TGlyphInfo;
  TGlyphInfo = record
    Bitmap: TBitmap;
    Adjustment: TGlyphAdjustment;
    TransparentColor: TColor;
  end;
  
  { TImageListEditorDlg }

  TImageListEditorDlg = class(TForm)
    BtnAdd: TButton;
    BtnClear: TButton;
    BtnDelete: TButton;
    BtnMoveUp: TButton;
    BtnMoveDown: TButton;
    BtnSave: TButton;
    btnSaveAll: TButton;
    BtnPanel: TButtonPanel;
    ColorBoxTransparent: TColorBox;
    GroupBoxL: TGroupBox;
    GroupBoxR: TGroupBox;
    ImageList: TImageList;
    LabelSize: TLabel;
    LabelTransparent: TLabel;
    OpenDialog: TOpenPictureDialog;
    RadioGroup: TRadioGroup;
    Preview: TScrollBox;
    SaveDialog: TSavePictureDialog;
    TreeView: TTreeView;
    procedure BtnAddClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnMoveUpClick(Sender: TObject);
    procedure btnSaveAllClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure ColorBoxTransparentClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PreviewPaint(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure TreeViewDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeViewSelectionChanged(Sender: TObject);
  private
    FImageList: TImageList;
    FModified: Boolean;
    FPreviewBmp: TBitmap;
    procedure SavePicture(Picture: TPicture);
  public
    procedure LoadFromImageList(AImageList: TImageList);
    procedure SaveToImageList;

    procedure AddImageToList(const FileName: String);
  end;

  //Editor call by Lazarus with 1 verbe only

  { TImageListComponentEditor }

  TImageListComponentEditor = class(TComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;


implementation

uses
  Math, GraphType;

{$R *.lfm}

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

{ TImageListEditorDlg }

procedure TImageListEditorDlg.FormCreate(Sender: TObject);
begin
  Caption := sccsILEdtCaption;

  GroupBoxL.Caption := sccsILEdtGrpLCaption;
  GroupBoxR.Caption := sccsILEdtGrpRCaption;

  BtnAdd.Caption := sccsILEdtAdd;
  BtnDelete.Caption := sccsILEdtDelete;
  BtnClear.Caption := sccsILEdtClear;
  BtnMoveUp.Caption := sccsILEdtMoveUp;
  BtnMoveDown.Caption := sccsILEdtMoveDown;
  BtnSave.Caption := sccsILEdtSave;
  BtnSaveAll.Caption := sccsILEdtSaveAll;

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
end;

procedure TImageListEditorDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPreviewBmp);
end;

procedure TImageListEditorDlg.BtnAddClick(Sender: TObject);
var
  I: Integer;
begin
  if OpenDialog.Execute then
  begin
    TreeView.BeginUpdate;
    try
      ImageList.BeginUpdate;
      try
        for I := 0 to OpenDialog.Files.Count - 1 do AddImageToList(OpenDialog.Files[I]);
      finally
        ImageList.EndUpdate;
      end;
    finally
      TreeView.EndUpdate;
    end;
    TreeView.SetFocus;
  end;
end;

procedure TImageListEditorDlg.BtnClearClick(Sender: TObject);
begin
  ImageList.Clear;
  TreeView.Items.Clear;
end;

procedure TImageListEditorDlg.BtnDeleteClick(Sender: TObject);
var
  Node: TTreeNode;
  I, S: Integer;
begin
  if Assigned(TreeView.Selected) then
  begin
    Node := TreeView.Selected.GetNext;
    if Node = nil then Node := TreeView.Selected.GetPrev;

    S := TreeView.Selected.ImageIndex;
    ImageList.Delete(S);
    TreeView.BeginUpdate;
    try
      TreeView.Selected.Delete;
      
      for I := S to TreeView.Items.Count -1 do
      begin
        TreeView.Items[I].Text := IntToStr(I);
        TreeView.Items[I].ImageIndex := I;
        TreeView.Items[I].SelectedIndex := I;
      end;
    finally
      TreeView.EndUpdate;
    end;
    TreeView.Selected := Node;
  end;
  TreeView.SetFocus;
end;

procedure TImageListEditorDlg.BtnMoveUpClick(Sender: TObject);
var
  S, D: Integer;
  P: PGlyphInfo;
begin
  if Assigned(TreeView.Selected) and (TreeView.Items.Count > 1) then
  begin
    S := TreeView.Selected.ImageIndex;
    D := (Sender as TControl).Tag;
    if (S + D >= 0) and (S + D < TreeView.Items.Count) then
    begin
      ImageList.Move(S, S + D);
      
      P := TreeView.Items[S + D].Data;
      TreeView.Items[S + D].Data := TreeView.Items[S].Data;
      TreeView.Items[S].Data := P;
      
      TreeView.Selected := TreeView.Items[S + D];
      TreeView.SetFocus;
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
  if Assigned(TreeView.Selected) then
  begin
    Picture := TPicture.Create;
    try
      ImageList.GetBitmap(TreeView.Selected.ImageIndex, Picture.Bitmap);
      SavePicture(Picture);
    finally
      Picture.Free;
    end;
  end;
end;

procedure TImageListEditorDlg.ColorBoxTransparentClick(Sender: TObject);
var
  P: PGlyphInfo;
  T: TBitmap;
begin
  if Assigned(TreeView.Selected) then
  begin
    if Assigned(TreeView.Selected.Data) then
    begin
      P := PGlyphInfo(TreeView.Selected.Data);
      P^.Adjustment := TGlyphAdjustment(RadioGroup.ItemIndex);
      P^.TransparentColor := ColorBoxTransparent.Selected;
      
      T := CreateGlyph(P^.Bitmap, ImageList.Width, ImageList.Height, P^.Adjustment,
        P^.TransparentColor);
      ImageList.BeginUpdate;
      try
        ImageList.Delete(TreeView.Selected.ImageIndex);
        ImageList.Insert(TreeView.Selected.ImageIndex, T, nil);
      finally
        ImageList.EndUpdate;
        T.Free;
      end;
      
      TreeView.Invalidate;
      TreeViewSelectionChanged(nil);
    end
  end;
end;

procedure TImageListEditorDlg.PreviewPaint(Sender: TObject);
begin
  if Assigned(FPreviewBmp) then
  begin
    Preview.Canvas.Draw(0, 0, FPreviewBmp);
  end;
end;

procedure TImageListEditorDlg.btnApplyClick(Sender: TObject);
begin
  SaveToImageList;
end;

procedure TImageListEditorDlg.TreeViewDeletion(Sender: TObject; Node: TTreeNode);
var
  P: PGlyphInfo;
begin
  if Assigned(Node) then
  begin
    if Node.Data <> nil then
    begin
      P := PGlyphInfo(Node.Data);
      P^.Bitmap.Free;
      Dispose(P);
    end;
  end;
end;

procedure TImageListEditorDlg.TreeViewSelectionChanged(Sender: TObject);
var
  P: PGlyphInfo;
begin
  if Assigned(TreeView.Selected) then
  begin
    if Assigned(FPreviewBmp) then FPreviewBmp.Free;
    FPreviewBmp := TBitmap.Create;
    ImageList.GetBitmap(TreeView.Selected.ImageIndex, FPreviewBmp);

    if Assigned(TreeView.Selected.Data) then
    begin
      P := PGlyphInfo(TreeView.Selected.Data);
      
      RadioGroup.Enabled := True;
      RadioGroup.OnClick := nil;
      RadioGroup.ItemIndex := Integer(P^.Adjustment);
      RadioGroup.OnClick := @ColorBoxTransparentClick;
      
      ColorBoxTransparent.Enabled := True;
      ColorBoxTransparent.OnChange := nil;
      ColorBoxTransparent.Selected := P^.TransparentColor;
      ColorBoxTransparent.OnChange := @ColorBoxTransparentClick;
    end
    else
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
    
    LabelSize.Caption := Format('%d x %d', [FPreviewBmp.Width, FPreviewBmp.Height]);

    Preview.HorzScrollBar.Range := FPreviewBmp.Width;
    Preview.VertScrollBar.Range := FPreviewBmp.Height;
    Preview.Invalidate;
  end
  else
  begin
    if Assigned(FPreviewBmp) then FreeThenNil(FPreviewBmp);
    LabelSize.Caption := '';
    
    RadioGroup.Enabled := False;
    RadioGroup.OnClick := nil;
    RadioGroup.ItemIndex := 0;
    RadioGroup.OnClick := @ColorBoxTransparentClick;

    ColorBoxTransparent.Enabled := False;
    ColorBoxTransparent.OnChange := nil;
    ColorBoxTransparent.Selected := clFuchsia;
    ColorBoxTransparent.OnChange := @ColorBoxTransparentClick;
      
    Preview.HorzScrollBar.Range := ImageList.Width;
    Preview.VertScrollBar.Range := ImageList.Height;
    Preview.Invalidate;
  end;
end;

procedure TImageListEditorDlg.LoadFromImageList(AImageList: TImageList);
var
  I, C: Integer;
begin
  ImageList.Clear;
  FImageList := AImageList;
  FModified := False;
    
  if Assigned(AImageList) then
  begin
    ImageList.Assign(AImageList);

    C := ImageList.Count;

    TreeView.BeginUpdate;
    try
      TreeView.Items.Clear;
      for I := 0 to Pred(C) do
      begin
        with TreeView.Items.Add(nil, IntToStr(I)) do
        begin
          ImageIndex := I;
          SelectedIndex := I;
          Data := nil;
        end;
      end;
    finally
      TreeView.EndUpdate;
    end;
  end;
end;

procedure TImageListEditorDlg.SaveToImageList;
begin
  FImageList.Assign(ImageList);
  FModified := True;
end;

procedure TImageListEditorDlg.AddImageToList(const FileName: String);
var
  SrcBmp: TBitmap;
  Picture: TPicture;
  Node: TTreeNode;
  P: PGlyphInfo;
  I: Integer;
  ImagesPerColumn: Integer;
  ImagesPerRow: Integer;
  iRow: Integer;
  iCol: Integer;
begin
  SaveDialog.InitialDir := ExtractFileDir(FileName);
  SrcBmp := nil;
  
  Picture := TPicture.Create;
  try
    Picture.LoadFromFile(FileName);
    SrcBmp := TBitmap.Create;
    SrcBmp.Assign(Picture.Graphic);
  finally
    Picture.Free;
  end;

  if Assigned(SrcBmp) then
  begin
    if not SrcBmp.Empty then
    begin
      //If the height and with of SrcBmp is an exact factor of ImageList height and width
      //the image can be split into smaller images
      if (SrcBmp.Height mod ImageList.Height = 0)
        and (SrcBmp.Width mod ImageList.Width = 0) then
      begin
        ImagesPerColumn := SrcBmp.Height div ImageList.Height;
        ImagesPerRow := SrcBmp.Width div ImageList.Width;
      end
      else
      begin
        ImagesPerColumn := 1;
        ImagesPerRow := 1;
      end;
      //Ask the user if wants to split the source image
      if ((ImagesPerRow > 1) or (ImagesPerColumn > 1))
        and (IDEQuestionDialog(Caption +' - '+ btnAdd.Caption,
              s_SuggestSplitImage, mtConfirmation,
              [mrNo, s_AddAsSingle, mrYes, s_SplitImage]) <> mrYes) then
      begin
        //"Add as single" was choosen
        ImagesPerColumn := 1;
        ImagesPerRow := 1;
      end;
      //Split image or copy the first image list width/height image data if the file
      //is bigger than image list width/height but the user choosen "add as single"
      for iRow := 0 to ImagesPerColumn - 1 do
        for iCol := 0 to ImagesPerRow - 1 do
        begin
          New(P);
          P^.Bitmap := CreateGlyphSplit(SrcBmp, ImageList.Width, ImageList.Height,
            iRow, iCol);
          P^.Adjustment := gaNone;
          P^.TransparentColor := clDefault;

          I := ImageList.Add(P^.Bitmap, nil);
          Node := TreeView.Items.AddObject(nil, IntToStr(I), P);
          Node.ImageIndex := I;
          Node.SelectedIndex := I;
          TreeView.Selected := Node;
        end;
      SrcBmp.Free;
    end;
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
