unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnToggleEnabledDisabled: TButton;
    cbShowLines: TCheckBox;
    cbMultiSelect: TCheckBox;
    cbShowButtons: TCheckBox;
    cmbExpandSign: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    lbTask: TListBox;
    Panel1: TPanel;
    TreeView: TTreeView;
    procedure btnToggleEnabledDisabledClick(Sender: TObject);
    procedure cbMultiSelectChange(Sender: TObject);
    procedure cbShowButtonsChange(Sender: TObject);
    procedure cbShowLinesChange(Sender: TObject);
    procedure cmbExpandSignChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbTaskClick(Sender: TObject);

    // Here are the event handlers for several custom-drawing tasks

    // An image is drawn as an overall background of the treeview
    procedure BackgroundImage_AdvancedCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure BackgroundImage_AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);

    // A gradient is drawn as an overall background of the tree
    procedure BackgroundGradient_AdvancedCustomDraw(Sender: TCustomTreeView;
      const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure BackgroundGradient_AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);

    // The top-level nodes are painted with bold font
    procedure BoldTopLevel_CustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);

    // Selection and hot-tracked lines
    procedure RowSelectHotTrack_AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);

    // Selection and hot-tracked lines are drawn as a gradient
    procedure RowSelectHotTrackGradient_AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);

    // Selection and hot-tracked lines drawn with bitmap fill
    procedure SelectHotTrackImage_AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);

    // The top-level nodes are drawn with a gradient
    procedure TopLevelGradient_AdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);

  private
    FBackImg, FSelectionImg, FHotTrackImg: TPicture;
    procedure PopulateTree;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  tDefault_Themed = 0;
  tDefault_NotThemed = 1;
  tDefault_HotTrack_Themed = 2;
  tDefault_HotTrack_NotThemed = 3;
  tTopLevelBold = 4;
  tTopLevelGradient = 5;
  tRowSelect_HotTrack_Full = 6;
  tRowSelect_HotTrack_Icon = 7;
  tRowSelect_HotTrack_Text = 8;
  tRowSelect_HotTrack_Gradient_Full = 9;
  tRowSelect_HotTrack_Gradient_Icon = 10;
  tRowSelect_HotTrack_Gradient_Text = 11;
  tRowSelect_HotTrack_Gradient_TextOnly = 12;
  tSelectHotTrack_Image = 13;
  tBackgroundImage_Themed = 14;
  tBackgroundImage_NotThemed = 15;
  tBackgroundImage_Themed_NoHotTrackIcons = 16;
  tBackgroundGradient_Themed = 17;
  tBackgroundGradient_NotThemed = 18;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PopulateTree;
  TreeView.Selected := TreeView.Items.GetFirstNode;

  FBackImg := TPicture.Create;
  FBackImg.LoadFromFile(Application.Location + 'paw-gray.png');

  FSelectionImg := TPicture.Create;
  FSelectionImg.LoadFromFile(Application.Location + 'water1.jpg');

  FHotTrackImg := TPicture.Create;
  FHotTrackImg.LoadFromFile(Application.Location + 'water2.jpg');

  lbTask.ItemIndex := 0;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FBackImg.Free;
  FSelectionImg.Free;
  FHotTrackImg.Free;
end;

procedure TMainForm.btnToggleEnabledDisabledClick(Sender: TObject);
var
  node: TTreeNode;
begin
  node := TreeView.Items.GetFirstNode;
  node.Enabled := not node.Enabled;
end;

procedure TMainForm.cbMultiSelectChange(Sender: TObject);
begin
  if cbMultiSelect.Checked then
    TreeView.Options := TreeView.Options + [tvoAllowMultiSelect]
  else
    TreeView.Options := TreeView.Options - [tvoAllowMultiSelect];
end;

procedure TMainForm.cbShowButtonsChange(Sender: TObject);
begin
  if cbShowButtons.Checked then
    TreeView.Options := TreeView.Options + [tvoShowButtons]
  else
    TreeView.Options := TreeView.Options - [tvoShowButtons];
end;

procedure TMainForm.cbShowLinesChange(Sender: TObject);
begin
  if cbShowLines.Checked then
    TreeView.Options := TreeView.Options + [tvoShowLines]
  else
    TreeView.Options := TreeView.Options - [tvoShowLines];
end;

procedure TMainForm.cmbExpandSignChange(Sender: TObject);
begin
  TreeView.ExpandSignType := TTreeViewExpandSignType(cmbExpandSign.ItemIndex);
end;

procedure TMainForm.PopulateTree;

  function AddChild(AParent: TTreeNode; ACaption: String; AImgIndex: Integer): TTreeNode;
  begin
    Result := TreeView.Items.AddChild(AParent, ACaption);
    Result.ImageIndex := AImgIndex;
    Result.SelectedIndex := AImgIndex;
  end;

  function AddSibling(ANode: TTreeNode; ACaption: String; AImgIndex: Integer): TTreeNode;
  begin
    Result := TreeView.Items.Add(ANode, ACaption);
    Result.ImageIndex := AImgIndex;
    Result.SelectedIndex := AImgIndex;
  end;

var
  node: TTreeNode;
begin
  node := AddChild(nil, 'DateTimeCtrls', 0);
  AddChild(node, 'datetimectrls.pas', 1);
  AddChild(node, 'lclcalwrapper.pas', 1);

  node := AddChild(nil, 'DockedFormEditor', 0);
  AddChild(node, 'dockedformeditor.pas', 1);
  AddChild(node, 'dockedstrconsts.pas', 1);

  node := AddChild(nil, 'VirtualTreeView', 0);
  AddChild(node, 'laz.virtualtrees.pas', 1);
  AddChild(node, 'laz.lclconstants.inc', 1);
  AddChild(node, 'lazlclfunctions.inc', 1);
  AddChild(node, 'laz.registervirtualtreeview.pas', 1);

  TreeView.FullExpand;
end;

procedure TMainForm.lbTaskClick(Sender: TObject);
begin
  TreeView.OnCustomDraw := nil;
  TreeView.OnCustomDrawitem := nil;
  TreeView.OnAdvancedCustomDraw := nil;
  TreeView.OnAdvancedCustomDrawItem := nil;
  TreeView.Options := TreeView.Options + [tvoThemedDraw] - [tvoHotTrack, tvoRowSelect];

  // Select the event handler and options for the drawing task
  case lbTask.ItemIndex of
    tDefault_Themed:
      ;
    tDefault_NotThemed:
      TreeView.Options := TreeView.Options - [tvoThemedDraw];

    tDefault_HotTrack_Themed:
      TreeView.Options := TreeView.Options + [tvoHotTrack];
    tDefault_HotTrack_NotThemed:
      TreeView.Options := TreeView.Options - [tvoThemedDraw] + [tvoHotTrack];

    tTopLevelBold:
      TreeView.OnCustomDrawItem := @BoldTopLevel_CustomDrawItem;

    tTopLevelGradient:
      begin
        TreeView.Options := TreeView.Options - [tvoThemedDraw];
        TreeView.OnAdvancedCustomDrawItem := @TopLevelGradient_AdvancedCustomDrawItem;
      end;

    tRowSelect_HotTrack_Full,
    tRowSelect_HotTrack_Icon,
    tRowSelect_HotTrack_Text:
      begin
        // tvoRowSelect is not needed for this visual effect, but allows to
        // change the selection by clicking anywhere, not just on the node text.
        TreeView.Options := TreeView.Options + [tvoRowSelect, tvoHotTrack] - [tvoThemedDraw];
        TreeView.OnAdvancedCustomDrawItem := @RowSelectHotTrack_AdvancedCustomDrawItem;
      end;

    tRowSelect_HotTrack_Gradient_Full,
    tRowSelect_HotTrack_Gradient_Icon,
    tRowSelect_HotTrack_Gradient_Text,
    tRowSelect_HotTrack_Gradient_TextOnly:
      begin
        // tvoRowSelect is not needed for this visual effect, but allows to
        // change the selection by clicking anywhere, not just on the node text.
        TreeView.Options := TreeView.Options + [tvoRowSelect, tvoHotTrack] - [tvoThemedDraw];
        TreeView.OnAdvancedCustomDrawItem := @RowSelectHotTrackGradient_AdvancedCustomDrawItem;
      end;

    tSelectHotTrack_Image:
      begin
        TreeView.Options := TreeView.Options + [tvoHotTrack] - [tvoThemedDraw];
        TreeView.OnAdvancedCustomDrawItem := @SelectHotTrackImage_AdvancedCustomDrawItem;
      end;

    tBackgroundImage_Themed,
    tBackgroundImage_Themed_NoHotTrackIcons:
      begin
        TreeView.Options := TreeView.Options + [tvoHotTrack];
        TreeView.OnAdvancedCustomDraw := @BackgroundImage_AdvancedCustomDraw;
        TreeView.OnAdvancedCustomDrawItem := @BackgroundImage_AdvancedCustomDrawItem;
      end;
    tBackgroundImage_NotThemed:
      begin
        TreeView.Options := TreeView.Options + [tvoHotTrack] - [tvoThemedDraw];
        TreeView.OnAdvancedCustomDraw := @BackgroundImage_AdvancedCustomDraw;
        TreeView.OnAdvancedCustomDrawItem := @BackgroundImage_AdvancedCustomDrawItem;
      end;

    tBackgroundGradient_Themed:
      begin
        TreeView.Options := TreeView.Options + [tvoHotTrack];
        TreeView.OnAdvancedCustomDraw := @BackgroundGradient_AdvancedCustomDraw;
        TreeView.OnAdvancedCustomDrawItem := @BackgroundGradient_AdvancedCustomDrawItem;
      end;
    tBackgroundGradient_NotThemed:
      begin
        TreeView.Options := TreeView.Options + [tvoHotTrack] - [tvoThemedDraw];
        TreeView.OnAdvancedCustomDraw := @BackgroundGradient_AdvancedCustomDraw;
        TreeView.OnAdvancedCustomDrawItem := @BackgroundGradient_AdvancedCustomDrawItem;
      end;
  end;
  TreeView.Invalidate;
end;

procedure TMainForm.BackgroundGradient_AdvancedCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if Stage = cdPrePaint then
  begin
    // Draw the gradient
    Sender.Canvas.GradientFill(ARect, clSkyBlue, clWhite, gdVertical);
    // Avoid painting the normal background below the last node
    Sender.Canvas.Brush.Color := clNone;
  end;
  // We must not set DefaultDraw to false here because the CustomDraw
  // PrePaint stage would force us to paint everything by ourselves.
end;

procedure TMainForm.BackgroundGradient_AdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
begin
  case Stage of
    cdPreErase:
      // Avoid overwriting the gradient with the node background color
      Sender.Canvas.Brush.Color := clNone;
    cdPostErase:
      // Set selected and hot-track color as usual
      if ([cdsFocused, cdsSelected] * State <> []) then
      begin
        Sender.Canvas.Brush.Color := clNavy;
        Sender.Canvas.Font.Color := clYellow;
        Sender.Canvas.Font.Style := [fsBold];
      end else
      if (cdsHot in State) then
      begin
        Sender.Canvas.Brush.Color := clGray;
        Sender.Canvas.Font.Color := clHighlightText;
        Sender.Canvas.Font.Style := [];
      end;
  end;
end;

procedure TMainForm.BackgroundImage_AdvancedCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  x, y: Integer;
begin
  if Stage = cdPrePaint then
  begin
    // Draw the image
    y := 0;
    while (y < TreeView.ClientHeight) do
    begin
      x := 0;
      while (x < TreeView.ClientWidth) do
      begin
        TreeView.Canvas.Draw(x, y, FBackImg.Graphic);
        inc(x, FBackImg.Width);
      end;
      inc(y, FBackImg.Height);
    end;
    // Avoid painting the normal background below the last node
    Sender.Canvas.Brush.Color := clNone;
    // We must not set DefaultDraw to false here because the CustomDraw
    // PrePaint stage would force us to paint everything by ourselves.
  end;
end;

procedure TMainForm.BackgroundImage_AdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
begin
  case Stage of
    cdPreErase:
      // Avoid overwriting the image with the node background color
      Sender.Canvas.Brush.Color := clNone;
    cdPostErase:
      // Avoid drawing the default hottrack node text background over the image
      if (cdsSelected in State) then
        Sender.Canvas.Brush.Color := clHighlight
      else
      if (cdsHot in State) then
      begin
        Sender.Canvas.Brush.Color := clNone;
        if lbTask.ItemIndex = tBackgroundImage_Themed_NoHotTrackIcons then
          PaintImages := false;
      end;
  end;
end;

// This OnCustomDrawItem handler uses a bold font to draw the top-level tree nodes
procedure TMainForm.BoldTopLevel_CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.Level = 0 then
    Sender.Canvas.Font.Style := [fsBold]
  else
    Sender.Canvas.Font.Style := [];
end;

{ Full row highlighting. Depending on the selection in the Task radiogroup,
  there are three possibilities:
  - highlighting across the entire row
  - highlighting starting at the icon
  - highlighting starting at the node text. }
procedure TMainForm.RowSelectHotTrack_AdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  R: TRect;
  idx: Integer;
begin
  if Stage = cdPostErase then
  begin
    if [cdsFocused, cdsSelected] * State <> [] then    // Selected node
      Sender.Canvas.Brush.Color := clRed
    else
    if cdsHot in State then                            // Hot-tracked node
      Sender.Canvas.Brush.Color := $ccccff
    else
      Sender.Canvas.Brush.Color := clWindow;           // Normal nodes
    idx := lbTask.ItemIndex;
    case lbTask.ItemIndex of
      tRowSelect_HotTrack_Full:           // full with of displayed node
        DefaultDraw := true;
      tRowSelect_HotTrack_Icon:           // beginning at icon
        begin
          R := Node.DisplayRect(false);
          R.Left := Node.DisplayIconLeft;
          Sender.Canvas.FillRect(R);
          // do not draw the default full width highlight:
          DefaultDraw := false;
        end;
      tRowSelect_HotTrack_Text:           // beginning at node text
        begin
          R := Node.DisplayRect(false);
          R.Left := Node.DisplayTextLeft;
          Sender.Canvas.FillRect(R);
          // do not draw the default full width highlight:
          DefaultDraw := false;
        end;
    end;
  end;

  // Turn off hot-track underlining
  Sender.Canvas.Font.Style := [];
end;

{ Full row highlighting by a gradient.
  Depending on the selection in the Task radiogroup, there are three possibilities:
  - highlighting across the entire row
  - highlighting starting at the icon
  - highlighting starting at the node text. }
procedure TMainForm.RowSelectHotTrackGradient_AdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  R: TRect;
  startColor, endColor: TColor;
begin
  if Stage = cdPostErase then
  begin
    if [cdsFocused, cdsSelected] * State <> [] then    // Selected node
    begin
      startColor := clRed;
      endColor := $ccccff;
    end else
    if cdsHot in State then                            // Hot-tracked node
    begin
      startColor := $ccccff;
      endColor := clWhite;
    end else
    begin
      Sender.Canvas.Brush.Color := clWindow;  // Normal nodes
      DefaultDraw := true;
      exit;
    end;

    R := Node.DisplayRect(false);
    case lbTask.ItemIndex of
      tRowSelect_HotTrack_Gradient_Full:      // Full with of displayed node
        ;
      tRowSelect_HotTrack_Gradient_Icon:      // Beginning at icon
        R.Left := Node.DisplayIconLeft;
      tRowSelect_HotTrack_Gradient_Text:      // Beginning at node text
        R.Left := Node.DisplayTextLeft;
      tRowSelect_HotTrack_Gradient_TextOnly:  // Node text only
        R := Node.DisplayRect(true);
    end;
    Sender.Canvas.GradientFill(R, startColor, endColor, gdHorizontal);

    // Avoid drawing the standard node text highlight background
    Sender.Canvas.Brush.Style := bsClear;
    //or: Sender.Canvas.Brush.Color := clNone;

    // Turn off the default full-row highlighting (just for completeness,
    // is turned off by the clear brush style anyway).
    DefaultDraw := false;
  end;

  // Turn off hot-track underlining
  Sender.Canvas.Font.Style := [];
end;

procedure TMainForm.SelectHotTrackImage_AdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  R: TRect;
begin
  if Stage = cdPostErase then
  begin
    if [cdsFocused, cdsSelected] * State <> [] then    // Selected node
    begin
      Sender.Canvas.Brush.Style := bsImage;
      Sender.Canvas.Brush.Bitmap := FSelectionImg.Bitmap;
    end else
    if cdsHot in State then                            // Hot-tracked node
    begin
      Sender.Canvas.Brush.Style := bsImage;
      Sender.Canvas.Brush.Bitmap := FHotTrackImg.Bitmap;
    end else
      Sender.Canvas.Brush.Color := clWindow;           // Normal nodes
    R := Node.DisplayRect(true);
    Sender.Canvas.FillRect(R);

    // Avoid drawing the standard node text highlight background
    Sender.Canvas.Brush.Style := bsClear;

    // Turn off the default full-row highlighting (just for completeness,
    // is turned off by the clear brush style anyway).
    DefaultDraw := false;
  end;

  // Turn off hot-track underlining
  Sender.Canvas.Font.Style := [];
end;

procedure TMainForm.TopLevelGradient_AdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  R: TRect;
begin
  if Stage = cdPostErase then
  begin
    R := Node.DisplayRect(false);
    if Node.Level = 0 then
    begin
      Sender.Canvas.Brush.Color := clNone;
      Sender.Canvas.GradientFill(R, clSkyBlue, clWhite, gdHorizontal);
      DefaultDraw := false;
    end;
  end;
end;

end.

