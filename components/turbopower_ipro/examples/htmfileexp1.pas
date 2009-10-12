unit HtmFileExp1;

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
  IpUtils, IpHtml, ExtCtrls, StdCtrls, FileUtil;

type
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
  end;

  TPst = class(TObject)
    Position: Integer;
  end;

  { TFHtmFileExp1 }

  TFHtmFileExp1 = class(TForm)
    B_OpenHTMLFile: TButton;
    IpHtmlPanel1: TIpHtmlPanel;
    OpenDialog1: TOpenDialog;
    P_Top: TPanel;
    SB_GoBackward: TSpeedButton;
    SB_GoForward: TSpeedButton;
    procedure B_OpenHTMLFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure IpHtmlPanel1HotClick(Sender: TObject);
    procedure SB_GoBackwardClick(Sender: TObject);
    procedure SB_GoForwardClick(Sender: TObject);
  private
    SL: TStringList;
    CurrPos: Integer;
    CurrFile: string;
    Path: string;
    PathChanged: Boolean;
    procedure GoBackFor (GoBack: Boolean);
    procedure OpenHTMLFile(const Filename: string;
      ToAdd, RelativePath: Boolean);
  end;

var
  FHtmFileExp1: TFHtmFileExp1;

function ProgramDirectory(BundleRoot: boolean): string;

implementation

function ProgramDirectory(BundleRoot: boolean): string;
const
  BundlePostFix='.app/Contents/MacOS';
begin
  Result:=FileUtil.ProgramDirectory;
  if BundleRoot
  and (RightStr(ChompPathDelim(Result),length(BundlePostFix))=BundlePostFix) then
    Result:=ExtractFilePath(LeftStr(Result,length(Result)-length(BundlePostFix)));
end;


{--------------------------------------}
{-EVENTS-----------}

procedure TFHtmFileExp1.FormCreate(Sender: TObject);
var
  Dir: String;
  Filename: String;
begin
  SL := TStringList.Create;
  CurrPos := -1;
  Dir:=ProgramDirectory(false);
  SetCurrentDirUTF8(Dir);
  Filename:=ParamStrUTF8(1);
  if not FileExistsUTF8(Filename) then
    Filename:='index.html';
  OpenHTMLFile (Filename, True, False);
end;

procedure TFHtmFileExp1.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  for  I := SL.Count-1 downto 0  do
    TPst(SL.Objects[I]).Free;
  SL.Free;
end;

procedure TFHtmFileExp1.B_OpenHTMLFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    OpenHtmlFile (OpenDialog1.FileName, True, False);
  end;
end;

procedure TFHtmFileExp1.HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
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

procedure TFHtmFileExp1.IpHtmlPanel1HotClick(Sender: TObject);
begin
  if  IpHtmlPanel1.HotNode is TIpHtmlNodeA  then  begin
    TPst(SL.Objects[CurrPos]).Position := IpHtmlPanel1.VScrollPos;
    OpenHTMLFile (TIpHtmlNodeA(IpHtmlPanel1.HotNode).HRef, True, True);
  end;
end;

procedure TFHtmFileExp1.SB_GoBackwardClick(Sender: TObject);
begin
  TPst(SL.Objects[CurrPos]).Position := IpHtmlPanel1.VScrollPos;
  GoBackFor (True);
end;

procedure TFHtmFileExp1.SB_GoForwardClick(Sender: TObject);
begin
  GoBackFor (False);
end;

{-PRIVATE----------}

procedure TFHtmFileExp1.GoBackFor (GoBack: Boolean);
var
  Pst: TPst;
  S: string;
  SameFile: Boolean;
begin
  if GoBack
  then  Dec (CurrPos)
  else  Inc (CurrPos);
  if GoBack then  begin
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
  IpHtmlPanel1.VScrollPos := Pst.Position;
  SB_GoBackward.Enabled := (SL.Count > 1) and (CurrPos > 0);
  SB_GoForward.Enabled := (SL.Count > 1) and (CurrPos < SL.Count-1);
end {GoBackFor};

procedure TFHtmFileExp1.OpenHTMLFile(const Filename: string;
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
    Pst.Position := IpHtmlPanel1.VScrollPos;
    for  I := SL.Count-1 downto CurrPos+1  do  begin
      TPst(SL.Objects[I]).Free;
      SL.Delete(I);
    end;
    CurrPos := SL.AddObject (FN, Pst);
    SB_GoBackward.Enabled := SL.Count > 1;
    SB_GoForward.Enabled := False;
  end;
end {UpdateSB};

var
  fs: TFileStream;
  NewHTML: TSimpleIpHtml;
  P: Integer;
begin
  if  Filename = ''  then  begin
    if  CurrPos > -1
    then  IpHtmlPanel1.VScrollPos := 0;
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
      IpHtmlPanel1.MakeAnchorVisible (Anchor);
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
      NewHTML.LoadFromStream (fs);
      IpHtmlPanel1.SetHtml (NewHTML);
      if  Anchor <> ''
      then  IpHtmlPanel1.MakeAnchorVisible (Anchor);
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

initialization
  {$I htmfileexp1.lrs}
  {$I defaultimage.lrs}

end.

