{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Abstract:
    Mini-Map panel control
}
unit PnlMiniMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, SynEdit, SrcEditorIntf, Graphics, lclType,
  SynEditMarkupSpecialLine, SynEditTypes, SynEditMiscClasses, SynEditMarkupBracket, LazLogger,
  LazLoggerBase, LazEditTextAttributes;

Const
  DefaultViewFontSize        = 3;
  DefaultViewWindowColor     = TColor($00E3F33F);
  DefaultViewWindowTextColor = clNone;
  DefaultMapWidth            = 200;

Type

  { TMiniMapControl }

  TMiniMapControl = Class(TPanel)
  Private
    FMiniSynEdit: TSynEdit;
    FSourceEditor: TSourceEditorInterface;
    FSourceSynEdit: TCustomSynEdit;
    FViewWindowColor:TColor;
    FViewWindowTextColor:TColor;
    FViewFontSize: Integer;
    procedure ConfigMiniEdit;
    procedure SetSourceEditor(AValue: TSourceEditorInterface);
    procedure SetViewFontSize(AValue: Integer);
    procedure SetViewWindowColor(AValue: TColor);
    procedure SetViewWindowTextColor(AValue: TColor);
    procedure SyncMiniMapProps;
  Protected
    // Event handlers
    procedure HandleLineMarkup(Sender: TObject; const Info: TSpecialLineMarkupExInfo; var Special: boolean; Markup: TLazEditTextAttributeModifier); virtual;
    procedure HandleStatusChange(Sender: TObject; {%H-}Changes: TSynStatusChanges); virtual;
    Procedure HandleClick({%H-}aSender : TObject); virtual;
    Procedure SyncViewWindow;
    Property MiniSynEdit : TSynEdit Read FMiniSynEdit;
    Property SourceSynEdit : TCustomSynEdit Read FSourceSynEdit;
  Public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    Procedure Reconfigure;
    procedure UnHook;
    Property SourceEditor: TSourceEditorInterface Read FSourceEditor Write SetSourceEditor;
    Property ViewWindowColor : TColor Read FViewWindowColor Write SetViewWindowColor;
    Property ViewWindowTextColor:TColor Read FViewWindowTextColor Write SetViewWindowTextColor;
    Property ViewFontSize : Integer Read FViewFontSize Write SetViewFontSize;
  end;

implementation

uses SynEditKeyCmds;

{ TMiniMapControl }

procedure TMiniMapControl.HandleLineMarkup(Sender: TObject; const Info: TSpecialLineMarkupExInfo;
  var Special: boolean; Markup: TLazEditTextAttributeModifier);

var
  TopLine,BottomLine: Integer;

begin
  TopLine:=FSourceSynEdit.TopLine;
  BottomLine:=FSourceSynEdit.BottomLine;
  if (Info.Line>=TopLine) and (Info.Line<=BottomLine) then
    begin
    Markup.Background:=FViewWindowColor;
    Markup.Foreground:=FViewWindowTextColor;
    Special:=True;
    end;
end;

procedure TMiniMapControl.HandleStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  SyncViewWindow;
end;

procedure TMiniMapControl.SetSourceEditor(AValue: TSourceEditorInterface);
begin
  if FSourceEditor=AValue then Exit;
  FSourceEditor:=AValue;
  FSourceSynEdit:=TCustomSynEdit(FSourceEditor.EditorControl);
  if Assigned(FSourceSynEdit) then
    begin
    SyncMiniMapProps;
    SyncViewWindow;
    end;
end;

procedure TMiniMapControl.SetViewFontSize(AValue: Integer);
begin
  if ViewFontSize=AValue then Exit;
  FViewFontSize:=AValue;
  FMiniSynEdit.Font.Size:=FViewFontSize;
end;

procedure TMiniMapControl.SetViewWindowColor(AValue: TColor);
begin
  if FViewWindowColor=AValue then Exit;
  FViewWindowColor:=AValue;
  FMiniSynEdit.Invalidate;
end;

procedure TMiniMapControl.SetViewWindowTextColor(AValue: TColor);
begin
  if FViewWindowTextColor=AValue then Exit;
  FViewWindowTextColor:=AValue;
  FMiniSynEdit.Invalidate;
end;

procedure TMiniMapControl.HandleClick(aSender: TObject);
begin
  SourceSynEdit.TopLine:=FMiniSynEdit.CaretY-(FSourceSynEdit.LinesInWindow div 2); //centered.
  SyncViewWindow;
end;

procedure TMiniMapControl.SyncViewWindow;

var
  CurrTop,CurrBottom : Integer;

begin
  if (FSourceSynEdit=nil) then
    Exit;
  CurrTop:=FSourceSynEdit.TopLine;
  CurrBottom:=FSourceSynEdit.BottomLine;
  if (CurrTop<FMiniSynEdit.TopLine) then
    FMiniSynEdit.TopLine:=CurrTop
  else if (CurrBottom>(FMiniSynEdit.TopLine+FMiniSynEdit.LinesInWindow)) then
    FMiniSynEdit.TopLine:=CurrBottom-FMiniSynEdit.LinesInWindow;
  FMiniSynEdit.Invalidate;
end;


procedure TMiniMapControl.SyncMiniMapProps;
begin
  With FMiniSynEdit do
    begin
    Font:=FSourceSynEdit.Font;
    Font.Size:=FViewFontSize;
    ShareTextBufferFrom(FSourceSynEdit);
    Highlighter:=FSourceSynEdit.Highlighter;
    RightEdge:=FSourceSynEdit.RightEdge;
    RightEdgeColor:=FSourceSynEdit.RightEdgeColor;
    Color:=FSourceSynEdit.Color;
    FSourceSynEdit.RegisterStatusChangedHandler(@HandleStatusChange,[scTopLine, scLinesInWindow]);
    end;
end;

procedure TMiniMapControl.ConfigMiniEdit;

  Procedure AddKey(aCommand : TSynEditorCommand; aShortCut : TShortCut);

  begin
    With FMiniSynEdit.Keystrokes.Add do
      begin
      Command:=aCommand;
      ShortCut:=aShortCut;
      end;
  end;

var
  I : integer;

begin
  With FMiniSynEdit do
    begin
    Left:=0;
    Top:=0;
    Align:=alClient;
    ParentColor:=False;
    ParentFont:=False;

    Font.Name := SynDefaultFontName;
    Font.Pitch := fpFixed;
    Font.Quality := fqNonAntialiased;
    Gutter.Visible := False;
    Gutter.Width := 57;
    RightGutter.Width := 0;
    VisibleSpecialChars := [vscSpace, vscTabAtLast]  ;
    SelectedColor.BackPriority := 50;
    SelectedColor.ForePriority := 50;
    SelectedColor.FramePriority := 50;
    SelectedColor.BoldPriority := 50;
    SelectedColor.ItalicPriority := 50;
    SelectedColor.UnderlinePriority := 50;
    SelectedColor.StrikeOutPriority := 50;
    BracketHighlightStyle := sbhsBoth;
    BracketMatchColor.Background := clNone;
    BracketMatchColor.Foreground := clNone;
    BracketMatchColor.Style := [fsBold];
    FoldedCodeColor.Background := clNone;
    FoldedCodeColor.Foreground := clGray;
    FoldedCodeColor.FrameColor := clGray;
    MouseLinkColor.Background := clNone;
    MouseLinkColor.Foreground := clBlue;
    LineHighlightColor.Background := clNone;
    LineHighlightColor.Foreground := clNone;
    end;
  SourceEditorManagerIntf.GetEditorControlSettings(FMiniSynEdit);
//  FMiniSynEdit.SelectionMode:=;
  FMiniSynEdit.Keystrokes.Clear;
  AddKey(ecUp,KeyToShortCut(VK_UP,[]));
  AddKey(ecScrollUp,KeyToShortCut(VK_UP,[ssCtrl]));
  AddKey(ecDown,KeyToShortCut(VK_DOWN,[ssCtrl]));
  AddKey(ecPageDown,KeyToShortCut(VK_NEXT,[]));
  AddKey(ecPageBottom,KeyToShortCut(VK_NEXT,[ssCtrl]));
  AddKey(ecPageUp,KeyToShortCut(VK_PRIOR,[]));
  AddKey(ecPageTop,KeyToShortCut(VK_PRIOR,[ssCtrl]));
  AddKey(ecEditorTop,KeyToShortCut(VK_HOME,[]));
  AddKey(ecEditorBottom,KeyToShortCut(VK_END,[]));

  FMiniSynEdit.Font.Size:=FViewFontSize;
  FMiniSynEdit.ReadOnly := True;
  FMiniSynEdit.Gutter.Visible := False;
  FMiniSynEdit.OnClick := @HandleClick;
  FMiniSynEdit.OnSpecialLineMarkupEx := @HandleLineMarkup;
  FMiniSynEdit.Options:=[eoNoCaret,eoNoSelection];
  FMiniSynEdit.Options2:=[];
  FMiniSynEdit.BookMarkOptions.EnableKeys:=False;
  FMiniSynEdit.BookMarkOptions.GlyphsVisible:=False;
  FMiniSynEdit.BookMarkOptions.DrawBookmarksFirst:=False;
  For I:=0 to FMiniSynEdit.Gutter.Parts.Count-1 do
    FMiniSynEdit.Gutter.Parts[I].Visible:=True;
//  FMiniSynEdit.Gutter.Parts[4].Visible := False; // code folding disabled.
end;


constructor TMiniMapControl.Create(aOwner: TComponent);
begin
  Inherited;
  BevelInner:=bvNone;
  BevelOuter:=bvNone;
  FMiniSynEdit:=TSynEdit.Create(Self);
  FMiniSynEdit.Parent:=Self;
  FViewFontSize:=DefaultViewFontSize;
  FViewWindowColor:=DefaultViewWindowColor;
  FViewWindowTextColor:=DefaultViewWindowTextColor;
  ConfigMiniEdit;
end;


procedure TMiniMapControl.UnHook;

begin
  if Not Assigned(FSourceSynEdit) then
    exit;
  FMiniSynedit.UnShareTextBuffer;
  FSourceSynEdit.UnRegisterStatusChangedHandler(@HandleStatusChange);
  FSourceSynEdit:=nil;
  FSourceEditor:=nil;
end;

destructor TMiniMapControl.Destroy;
begin
  Unhook;
  inherited destroy;
end;

procedure TMiniMapControl.Reconfigure;
begin
  FMiniSynEdit.Highlighter:=FSourceSynEdit.Highlighter;
end;

end.

