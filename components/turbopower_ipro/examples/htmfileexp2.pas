unit HtmFileExp2;

{$mode objfpc}{$H+}

{.$define UsePreview}
{$IFDEF LCL}
{$DEFINE IP_LAZARUS}
{$ENDIF}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  {$IFDEF IP_LAZARUS}
    {$ifdef UsePreview}
    OsPrinters,
    {$endif}
  {$ELSE}
    GIFImage,
    JPeg,
    ImageDLLLoader, PNGLoader, LinarBitmap, //from ImageFileLib of Michael Vinther: http://www.logicnet.dk/lib/
  {$ENDIF}
  IpHtml, ExtCtrls, StdCtrls, FileUtil;

type
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
  end;

  TPst = class(TObject)
    Position: Integer;
  end;

  TIpHtmlPanelH = class(TIpHtmlPanel)
  private
    SL: TStringList;
    CurrPos: Integer;
    CurrFile: string;
    Path: string;
    PathChanged: Boolean;
    FC_GoForward: TControl;
    FC_GoBackward: TControl;
    procedure GoBackFor (GotoBack: Boolean);
    procedure HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure HotClickH(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GoBackward;
    procedure GoForward;
    procedure OpenHTMLFile(const Filename: string;
      ToAdd, RelativePath: Boolean);
  published
    property C_GoBackward: TControl read FC_GoBackward write FC_GoBackward;
    property C_GoForward: TControl read FC_GoForward write FC_GoForward;
  end;

  TFHtmFileExp2 = class(TForm)
    B_OpenHTMLFile: TButton;
    OpenDialog1: TOpenDialog;
    P_Top: TPanel;
    SB_GoBackward: TSpeedButton;
    SB_GoForward: TSpeedButton;
    procedure B_OpenHTMLFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SB_GoBackwardClick(Sender: TObject);
    procedure SB_GoForwardClick(Sender: TObject);
  private
    IpHtmlPanel1: TIpHtmlPanelH;
  end;

var
  FHtmFileExp2: TFHtmFileExp2;

implementation

uses
  IpUtils;

{--------------------------------------}
{-PRIVATE----------}

procedure TIpHtmlPanelH.GoBackFor (GotoBack: Boolean);
var
  Pst: TPst;
  S: string;
  SameFile: Boolean;
begin
  if GotoBack
  then  Dec (CurrPos)
  else  Inc (CurrPos);
  if GotoBack then  begin
    SameFile := SL[CurrPos+1] = SL[CurrPos]
  end
  else  begin
    if  CurrPos > 0
    then  SameFile := SL[CurrPos-1] = SL[CurrPos]
    else  SameFile := False;
  end;
  if  SameFile
  then  S := ''
  else  S := SL[CurrPos];
  Pst := TPst(SL.Objects[CurrPos]);
  OpenHTMLFile (S, False, True);
  VScrollPos := Pst.Position;
  if  Assigned (C_GoBackward)
  then  C_GoBackward.Enabled := (SL.Count > 1) and (CurrPos > 0);
  if  Assigned (C_GoForward)
  then  C_GoForward.Enabled := (SL.Count > 1) and (CurrPos < SL.Count-1);
end {GoBackFor};

procedure TIpHtmlPanelH.HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
var
  PicCreated: Boolean;
  FN, nURL: string;
 {$IFNDEF IP_LAZARUS}
  Ext: string;
  BitMap: Graphics.TBitMap;
 {$ENDIF}
begin
  PicCreated := False;
  try
    if  PathChanged
    then  FN := Path
    else  FN := ExtractFilePath(SL[CurrPos]);
    if  Pos ('\',FN) <> 0
    then  nURL := NetToDOSPath(URL)
    else  nURL := URL;
    FN := Concat (FN, nURL);
    if FileExistsUTF8(FN) then begin
      if Picture = nil then begin
        Picture := TPicture.Create;
        PicCreated := True;
      end;
     {$IFNDEF IP_LAZARUS}
      Ext := LowerCase (Copy (ExtractFileExt (FN), 2, MaxInt));
      if  (Ext = 'bmp') or (Ext = 'emf') or (Ext = 'wmf') or (Ext = 'gif') or (Ext = 'jpg')  then  begin
     {$ENDIF}
        Picture.LoadFromFile(FN);
     {$IFNDEF IP_LAZARUS}
      end
      else  begin
        PicCreated := False;
        BitMap := Graphics.TBitMap.Create;
        with TLinearBitmap.Create do
          try
            LoadFromFile (FN);
            AssignTo (Bitmap);
            Picture.Bitmap.Assign (BitMap);
            PicCreated := True;
          finally
            Bitmap.Free;
            Free;
          end;
      end;
     {$ENDIF}
    end;
  except
    if PicCreated then
      Picture.Free;
    Picture := nil;
  end;
end {HTMLGetImageX};

procedure TIpHtmlPanelH.HotClickH(Sender: TObject);
begin
  if  HotNode is TIpHtmlNodeA  then  begin
    TPst(SL.Objects[CurrPos]).Position := VScrollPos;
    OpenHTMLFile (TIpHtmlNodeA(HotNode).HRef, True, True);
  end;
end;

{-PUBLIC-----------}

constructor TIpHtmlPanelH.Create(AOwner: TComponent);
begin
  inherited;
  SL := TStringList.Create;
  CurrPos := -1;
  OnHotClick := @HotClickH;
end;

destructor TIpHtmlPanelH.Destroy;
var
  I: Integer;
begin
  for  I := SL.Count-1 downto 0  do
    TPst(SL.Objects[I]).Free;
  SL.Free;
  inherited;
end {Destroy};

procedure TIpHtmlPanelH.GoBackward;
begin
  TPst(SL.Objects[CurrPos]).Position := VScrollPos;
  GoBackFor (True);
end;

procedure TIpHtmlPanelH.GoForward;
begin
  GoBackFor (False);
end;

procedure TIpHtmlPanelH.OpenHTMLFile(const Filename: string;
  ToAdd, RelativePath: Boolean);

var
  FN, Anchor: string;
  Pst: TPst;
procedure UpdateSB;
var
  I: Integer;
begin
  if ToAdd then  begin
    Pst := TPst.Create;
    Pst.Position := VScrollPos;
    for  I := SL.Count-1 downto CurrPos+1  do  begin
      TPst(SL.Objects[I]).Free;
      SL.Delete(I);
    end;
    CurrPos := SL.AddObject (FN, Pst);
    if  Assigned (C_GoBackward)
    then  C_GoBackward.Enabled := SL.Count > 1;
    if  Assigned (C_GoForward)
    then  C_GoForward.Enabled := False;
  end;
end {UpdateSB};

var
  fs: TFileStream;
  NewHTML: TSimpleIpHtml;
  P: Integer;
begin
  if  Filename = ''  then  begin
    if  CurrPos > -1
    then  VScrollPos := 0;
    Exit;
  end;
  P := Pos ('#', Filename);
  FN := Filename;
  if RelativePath then  begin
    PathChanged := False;
    if  P = 0  then  begin
      Anchor := '';
    end
    else if  P = 1  then  begin
      FN := Concat (Path, CurrFile);
      Anchor := Copy (Filename, 2, MaxInt);
      MakeAnchorVisible (Anchor);
      UpdateSB;
      Exit;
    end
    else  begin
      FN := Copy (Filename, 1, P-1);
      Anchor := Copy (Filename, P+1, MaxInt);
    end;
    if ToAdd then  begin
      FN := Concat (Path, FN);
    end;
  end
  else begin
    FN := ExpandFileNameUTF8(FN);
    CurrFile := ExtractFileName (FN);
    Path := ExtractFilePath (FN);
    PathChanged := True;
  end;
  try
    fs := TFileStream.Create (FN, fmOpenRead);
    try
      NewHTML := TSimpleIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
      NewHTML.OnGetImageX := @HTMLGetImageX;
      NewHTML.LoadFromStream(fs);
      SetHtml(NewHTML);
      if  Anchor <> ''
      then  MakeAnchorVisible (Anchor);
      UpdateSB;
    finally
      fs.Free;
    end;
  except
    on E: Exception do begin
      MessageDlg ('Unable to open HTML file'+sLineBreak+
        'HTML File: '+Filename+sLineBreak+
        'Error: '+E.Message, mtError, [mbCancel], 0);
    end;
  end;
end {OpenHTMLFile};

{--------------------------------------}
{-EVENTS-----------}

procedure TFHtmFileExp2.FormCreate(Sender: TObject);
begin
  IpHtmlPanel1 := TIpHtmlPanelH.Create (Application);
  with IpHtmlPanel1 do  begin
    Name := 'IpHtmlPanel';
    Parent := FHtmFileExp2;
    Align := alClient;
    FactBAParag := 0.5;
    C_GoBackward := SB_GoBackward;
    C_GoForward := SB_GoForward;
    OpenHTMLFile ('index.html', True, False);
  end;
end {FormCreate};

procedure TFHtmFileExp2.B_OpenHTMLFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    IpHtmlPanel1.OpenHtmlFile (OpenDialog1.FileName, True, False);
  end;
end;

procedure TFHtmFileExp2.SB_GoBackwardClick(Sender: TObject);
begin
  IpHtmlPanel1.GoBackward;
end;

procedure TFHtmFileExp2.SB_GoForwardClick(Sender: TObject);
begin
  IpHtmlPanel1.GoForward;
end;

{--------------------------------------}

initialization
  {$I htmfileexp2.lrs}
  {$I defaultimage.lrs}

end.

