{
 /***************************************************************************
                            postscriptprinter.pas
                            ---------------------

                               Printer object
                     Initial Revision  : Mon Nov 05 2002

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
 
  Author: Tony Maro
}

unit PostScriptPrinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, GraphType, Graphics, GraphMath, LCLIntf, Forms;
  
  // uses lcllinux or winapi for RGB conversions and FORMS for application object
  
  {
   Defines a special canvas type object and override drawing methods to make
   the postscript code...

   Defines a TPSPattern object that handles creation of patterns to be used
   in fills and paints

   TPostScript manages a list of patterns and inserts the definitions into the
   postscript code and manages when they are changed

   A pattern definition can access pattern definitions within the same
   postscript object, as long as the TPSPattern object pointer is placed into
   the canvas pen/brush at the time the new pattern is made
  }
  
type
  TPostScript = class;

  TPSPaintType = (ptColored, ptUncolored);
  TPSTileType = (ttConstant, ttNoDistortion, ttFast);
  TPostScriptCanvas = class; // forward reference

  { Remember, modifying a pattern affects that pattern for the ENTIRE document! }
  TPSPattern = class(TObject)
  private
    FOldName: String;
    FOnChange: TNotifyEvent;
    FBBox: TRect;
    FCanvas: TPostScriptCanvas;
    FName: String;
    FPaintType: TPSPaintType;
    FPostScript: TStringList;
    FTilingType: TPSTileType;
    FXStep: Real;
    FYStep: Real;
    function GetpostScript: TStringList;
    procedure SetBBox(const AValue: TRect);
    procedure SetName(const AValue: String);
    procedure SetPaintType(const AValue: TPSPaintType);
    procedure SetTilingType(const AValue: TPSTileType);
    procedure SetXStep(const AValue: Real);
    procedure SetYStep(const AValue: Real);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed;
    property BBox: TRect read FBBox write SetBBox;
    property PaintType: TPSPaintType read FPaintType write SetPaintType;
    property TilingType: TPSTileType read FTilingType write SetTilingType;
    property XStep: Real read FXStep write SetXStep;
    property YStep: Real read FYStep write SetYStep;
    property Name: String read FName write SetName;
    property Canvas: TPostScriptCanvas read FCanvas;
    property GetPS: TStringList read GetPostscript;
    property OldName: string read FOldName write FOldName; // used when notifying that name Changed
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  PPSPattern = ^TPSPattern; // used for array

  { basic pen object - modify later for better splitting of brush object }
  TPSObject = class(TObject)
  private
    FOnChange: TNotifyEvent;
  protected
    procedure Changed; virtual;
    procedure Lock;
    procedure UnLock;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { Pen and brush object both right now...}
  TPSPen = class(TPSObject)
  private
    FColor: TColor;
    FPattern: TPSPattern;
    FWidth: Real;
    procedure SetPattern(const AValue: TPSPattern);
  protected
    procedure SetColor(Value : TColor);
    procedure Setwidth(value : Real);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPSPen);
    property Color: TColor read FColor write SetColor;
    property Pattern: TPSPattern read FPattern write SetPattern;
    property Width: Real read FWidth write SetWidth;
    function AsString: String;
  end;


  { Custom canvas-like object that handles postscript code }
  TPostScriptCanvas = class(TObject)
  private
    FBrush: TPSPen;
    FFontFace: String;
    FFontSize: Integer;
    FHeight: Integer;
    FLineSpacing: Integer;
    FColor: TColor; // canvas color - implement later
    FPen: TPSPen;
    LastX: Integer;
    LastY: Integer;
    FPostScript: TStringList;
    function GetColor: TColor;
    procedure SetBrush(const AValue: TPSPen);
    procedure SetColor(const AValue: TColor);
    procedure SetFontFace(const AValue: String);
    procedure SetFontSize(const AValue: Integer);
    procedure SetPen(const AValue: TPSPen);
    function TranslateY(Ycoord: Integer): Integer; // Y axis is backwards in postscript
    procedure AddFill;
    procedure ResetPos; // reset back to last moveto location
    procedure PenChanged(Sender: TObject);
  public
    MPostScript: TPostScript;
    constructor Create(APostScript: TPostScript);
    destructor Destroy; override;
    procedure Clear;
    property PostScript: TStringList read FPostScript write FPostScript;
    property FontFace: String read FFontFace write SetFontFace;
    property FontSize: Integer read FFontSize write SetFontSize;
    property LineSpacing: Integer read FLineSpacing write FLineSpacing;
    procedure MoveTo(X1,Y1 : Integer);
    procedure LineTo(X1,Y1 : Integer);
    procedure Line(X1,Y1,X2,Y2 : Integer);
    procedure Rectangle(X1,Y1,X2,Y2 : Integer);
    procedure Rectangle(const Rect: TRect);
    procedure Polyline(Points: PPoint; NumPts: Integer);
    procedure Ellipse(x1, y1, x2, y2: Integer);
    procedure Ellipse(const Rect: TRect);
    procedure RadialPie(x,y,width,mheight,angle1,angle2 : Integer);
    //procedure Pie(x,y,width,height,SX,SY,EX,EY : Integer);
    procedure Writeln(const AString: String);
    procedure TextOut(X,Y: Integer; const Text: String);
    //procedure Chord(x,y,width,height,angle1,angle2 : Integer);
    //procedure Chord(x,y,width,height,SX,SY,EX,EY : Integer);
    //procedure PolyBezier(Points: PPoint; NumPts: Integer;
    //                     Filled: boolean{$IFNDEF VER1_0} = False{$ENDIF};
    //                     Continuous: boolean{$IFNDEF VER1_0} = False{$ENDIF});
    //procedure PolyBezier(const Points: array of TPoint;
    //                     Filled: boolean{$IFNDEF VER1_0} = False{$ENDIF};
    //                     Continuous: boolean{$IFNDEF VER1_0} = False{$ENDIF});
    //procedure PolyBezier(const Points: array of TPoint);
    //procedure Polygon(const Points: array of TPoint;
    //                  Winding: Boolean{$IFNDEF VER1_0} = False{$ENDIF};
    //                  StartIndex: Integer{$IFNDEF VER1_0} = 0{$ENDIF};
    //                  NumPts: Integer {$IFNDEF VER1_0} = -1{$ENDIF});
    //procedure Polygon(Points: PPoint; NumPts: Integer;
    //                  Winding: boolean{$IFNDEF VER1_0} = False{$ENDIF});
    //procedure Polygon(const Points: array of TPoint);
    //procedure FillRect(const Rect : TRect);
    //procedure FloodFill(X, Y: Integer; FillColor: TColor; FillStyle: TFillStyle);
    //procedure RoundRect(X1, Y1, X2, Y2: Integer; RX,RY : Integer);
    //procedure RoundRect(const Rect : TRect; RX,RY : Integer);
    property Height: Integer read FHeight write FHeight; // set so we can translate Y coords
    property Color: TColor read GetColor write SetColor;
    property Pen: TPSPen read FPen write SetPen;
    property Brush: TPSPen read FBrush write SetBrush;
  end;

  { Encapsulates ALL the postscript and uses the TPostScriptCanvas object for a single page }
  TPostScript = class(TObject)
  private
    FCanvas: TPostScriptCanvas;
    FHeight: Integer;
    FLineSpacing: Integer;
    FPageNumber: Integer;
    FTitle: String;
    FWidth: Integer;
    FDocument: TStringList;
    Patterns: PPSPattern;   // array of pointers to pattern objects
    NumPatterns: Integer; // number of patterns in array
    procedure SetHeight(const AValue: Integer);
    procedure SetLineSpacing(const AValue: Integer);
    procedure SetTitle(const AValue: String);
    procedure SetWidth(const AValue: Integer);
    procedure GrabCanvas;
    procedure UpdateBoundingBox;
    procedure PatternChanged(Sender: TObject);
    procedure InsertPattern(APattern: TPSPattern); // adds the pattern to the postscript
    procedure RemovePattern(APattern: TPSPattern); // remove the pattern from the postscript
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPattern(APSPattern: TPSPattern);
    function FindPattern(AName: String): TPSPattern;
    function DelPattern(AName: String): Boolean;
    function NewPattern(AName: String): TPSPattern;
    property Canvas: TPostScriptCanvas read FCanvas;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property Document: TStringList read FDocument;
    property PageNumber: Integer read FPageNumber;
    property Title: String read FTitle write SetTitle;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing;
    procedure BeginDoc;
    procedure NewPage;
    procedure EndDoc;
  end;


implementation


{ TPostScriptCanvas ----------------------------------------------------------}

{ Y coords in postscript are backwards... }
function TPostScriptCanvas.TranslateY(Ycoord: Integer): Integer;
begin
  Result := FHeight - Ycoord;
end;

{ Adds a fill finishing line to any path we desire to fill }
procedure TPostScriptCanvas.AddFill;
begin
  FPostScript.Add('gsave '+FBrush.AsString+' fill grestore');
end;

{ Sets the current font face}
procedure TPostScriptCanvas.SetFontFace(const AValue: String);
var
   MyString: String;
begin
  if FFontFace=AValue then exit;
  if pos(' ',AValue) > 0 then
  FFontFace := '('+AValue+')'
  else FFontFace:=AValue;

  MyString := '/'+FFontFace+' '+IntToStr(FFontSize)+' selectfont';
  // set the pen info
  
  FPostScript.Add(MyString);
end;


function TPostScriptCanvas.GetColor: TColor;
begin
  Result := FColor;
end;

procedure TPostScriptCanvas.SetBrush(const AValue: TPSPen);
begin
  if FBrush=AValue then exit;
  FBrush:=AValue;
end;

procedure TPostScriptCanvas.SetColor(const AValue: TColor);
begin
  FColor := AValue;
end;

procedure TPostScriptCanvas.SetFontSize(const AValue: Integer);
begin
  if FFontSize=AValue then exit;
  FFontSize:=AValue;
  FPostScript.Add('/'+FFontFace+' '+IntToStr(AValue)+' selectfont');
end;

procedure TPostScriptCanvas.SetPen(const AValue: TPSPen);
begin
  // change to ASSIGN method?
  if FPen=AValue then exit;
  FPen:=AValue;
end;


{ Return to last moveto location }
procedure TPostScriptCanvas.ResetPos;
begin
  // any routines that you specify a start location when calling such as
  // textout, ellipse, etc. should not affect the default cursor location.

  FPostScript.Add(IntToStr(LastX)+' '+IntToStr(TranslateY(LastY))+' moveto');
end;

{ This is called when drawing pen is Changed but NOT when brush changes }
procedure TPostScriptCanvas.PenChanged(Sender: TObject);
begin
  if FPostScript[FPostScript.Count-2] = '%%PEN' then begin
        // last operation was a pen, so delete it
        FPostScript.Delete(FPostScript.Count-1);
        FPostScript.Delete(FPostScript.Count-1);
  end;
  FPostScript.Add('%%PEN');
  FPostScript.Add(FPen.AsString);
end;

constructor TPostScriptCanvas.Create(APostScript: TPostScript);
begin
  MPostScript := APostScript;

  FPostScript := TStringList.Create;
  FHeight := 792; // length of page in points at 72 ppi

  // Choose a standard font in case the user doesn't
  FFontFace := 'AvantGarde-Book';
  SetFontSize(10);

  if Assigned(MPostScript) then begin
        FLineSpacing := MPostScript.LineSpacing;
  end;
     
  FPen := TPSPen.Create;
  FPen.Width := 1;
  FPen.Color := 0;
  FPen.OnChange := @PenChanged;
     
  FBrush := TPSPen.Create;
  FBrush.Width := 1;
  FBrush.Color := -1;
  // don't notify us that the brush Changed...
end;

destructor TPostScriptCanvas.Destroy;
begin
  FPostScript.Free;
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

{ Clear the postscript canvas AND the graphic canvas (Add later) }
procedure TPostScriptCanvas.clear;
begin
  // clear the canvas for the next page
  FPostScript.Clear;
  // Choose a standard font in case the user doesn't
  FPostScript.Add('/AvantGarde-Book findfont');
  FPostScript.Add('10 scalefont');
  FPostScript.Add('setfont');

  // also clear the canvas itself if we plan to embed the bitmap into
  // the postscript

  // also grab the latest canvas height just in case it's Changed
  FHeight := 792;
  if Assigned(MPostScript) then FHeight := MPostScript.Height;
end;

{ Move draw location }
procedure TPostScriptCanvas.MoveTo(X1, Y1: Integer);
var
   Y: Integer;
begin
  Y := TranslateY(Y1);
  FPostScript.Add(IntToStr(X1)+' '+IntToStr(Y)+' moveto');
  LastX := X1;
  LastY := Y1;
end;

{ Draw a line from current location to these coords }
procedure TPostScriptCanvas.LineTo(X1, Y1: Integer);
var
   Y: Integer;
begin
  Y := TranslateY(Y1);
  FPostScript.Add(IntToStr(X1)+' '+IntToStr(Y)+' lineto');
  LastX := X1;
  LastY := Y1;
end;

procedure TPostScriptCanvas.Line(X1, Y1, X2, Y2: Integer);
var
   Y12, Y22: Integer;
begin
  Y12 := TranslateY(Y1);
  Y22 := TranslateY(Y2);

  FPostScript.Add('newpath '+IntToStr(X1)+' '+IntToStr(Y12)+' moveto '+
        IntToStr(X2)+' '+IntToStr(Y22)+' lineto closepath stroke');

  // go back to last moveto position
  ResetPos;
end;

procedure TPostScriptCanvas.Rectangle(X1, Y1, X2, Y2: Integer);
var
   Y12, Y22: Integer;
begin
  Y12 := TranslateY(Y1);
  Y22 := TranslateY(Y2);

  FPostScript.Add('stroke newpath');
  FPostScript.Add(IntToStr(X1)+' '+IntToStr(Y12)+' moveto');
  FPostScript.Add(IntToStr(X2)+' '+IntToStr(Y12)+' lineto');
  FPostScript.Add(IntToStr(X2)+' '+IntToStr(Y22)+' lineto');
  FPostScript.Add(IntToStr(X1)+' '+IntToStr(Y22)+' lineto');
  FPostScript.Add('closepath');
  if FBrush.Color > -1 then AddFill;
  FPostScript.Add('stroke');
  ResetPos;
end;

{ Draw a rectangle }
procedure TPostScriptCanvas.Rectangle(const Rect: TRect);
var
   Y12, Y22: Integer;
begin
  Y12 := TranslateY(Rect.Top);
  Y22 := TranslateY(Rect.Bottom);

  FPostScript.Add('stroke newpath');
  FPostScript.Add(IntToStr(Rect.Left)+' '+IntToStr(Y12)+' moveto');
  FPostScript.Add(IntToStr(Rect.Right)+' '+IntToStr(Y12)+' lineto');
  FPostScript.Add(IntToStr(Rect.Right)+' '+IntToStr(Y22)+' lineto');
  FPostScript.Add(IntToStr(Rect.Left)+' '+IntToStr(Y22)+' lineto');
  FPostScript.Add('closepath');
  if FBrush.Color > -1 then AddFill;
  FPostScript.Add('stroke');
  ResetPos;
end;

{ Draw a series of lines }
procedure TPostScriptCanvas.Polyline(Points: PPoint; NumPts: Integer);
var
  i : Longint;
begin
  If (NumPts <= 1) or (Points = nil) then exit;

  MoveTo(Points[0].X, Points[0].Y);
  For i := 1 to NumPts - 1 do
    LineTo(Points[i].X, Points[i].Y);

  ResetPos;
end;

{ This was a pain to figure out... }
procedure TPostScriptCanvas.Ellipse(x1, y1, x2, y2: Integer);
var
   radius: Integer;
   YRatio: Real;
   centerX, centerY: Integer;
begin
     // set radius to half the width
  radius := (x2 - x1) div 2;

     //calculate ratios
  if radius <1 then exit; // do nothing
  YRatio := real(Y2 - Y1) / (X2-X1);

     // find center
  CenterX := ((X2 - X1) div 2) + X1;
  CenterY := ((Y2 - Y1) div 2) + Y1;

  FPostScript.Add('newpath '+IntToStr(CenterX)+' '+IntToStr(TranslateY(CenterY))+' translate');

     // move to edge
  FPostScript.Add(IntToStr(radius)+' 0 moveto');

     // now draw it
  FPostScript.Add('gsave 1 '+format('%.3f',[YRatio])+' scale');
  FPostScript.Add('0 0 '+IntToStr(radius)+' 0 360 arc');
  if FBrush.Color > -1 then AddFill;

     // reset scale for drawing line thickness so it doesn't warp
  YRatio := 1 / YRatio;
  FPostScript.Add('1 '+format('%.2f',[YRatio])+' scale stroke grestore');

     // move origin back
  FPostScript.Add(IntToStr(-CenterX)+' '+IntToStr(-TranslateY(CenterY))+' translate closepath stroke');
  ResetPos;
end;

procedure TPostScriptCanvas.Ellipse(const Rect: TRect);
begin
  self.Ellipse(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

procedure TPostScriptCanvas.RadialPie(x, y, width, mheight, angle1, angle2: Integer);
begin
  // set zero at center
  FPostScript.Add('newpath '+IntToStr(X)+' '+IntToStr(TranslateY(Y))+' translate');

  // scale it
  FPostScript.Add('gsave '+IntToStr(width)+' '+IntToStr(mheight)+' scale');
  //FPostScript.Add('gsave 1 1 scale');

  // draw line to edge
  FPostScript.Add('0 0 moveto');
  FPostScript.Add('0 0 1 '+IntToStr(angle1)+' '+IntToStr(angle2)+' arc closepath');

  if FBrush.Color > -1 then AddFill;

  // reset scale so we don't change the line thickness
  // adding 0.01 to compensate for scaling error - there may be a deeper problem here...
  FPostScript.Add(format('%.6f',[(real(1) / X)+0.01])+' '+format('%.6f',[(real(1) / Y)+0.01])+' scale stroke grestore');

  // close out and return origin
  FPostScript.Add(IntToStr(-X)+' '+IntToStr(-TranslateY(Y))+' translate closepath stroke');

  ResetPos;
end;

{ Writes text with a carriage return }
procedure TPostScriptCanvas.Writeln(const AString: String);
begin
  TextOut(LastX, LastY, AString);
  LastY := LastY+FFontSize+FLineSpacing;
  MoveTo(LastX, LastY);
end;


{ Output text, restoring draw location }
procedure TPostScriptCanvas.TextOut(X, Y: Integer; const Text: String);
var
  Y1: Integer;
begin
  Y1 := TranslateY(Y);
  FPostScript.Add(IntToStr(X)+' '+IntToStr(Y1)+' moveto');
  FPostScript.Add('('+Text+') show');
  ResetPos; // move back to last moveto location
end;

{ TPostScript -------------------------------------------------------------- }

procedure TPostScript.SetHeight(const AValue: Integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
  UpdateBoundingBox;
  // filter down to the canvas height property
  if assigned(FCanvas) then FCanvas.Height := FHeight;
end;

procedure TPostScript.SetLineSpacing(const AValue: Integer);
begin
  if FLineSpacing=AValue then exit;
  FLineSpacing:=AValue;
  // filter down to the canvas
  if assigned(FCanvas) then FCanvas.LineSpacing := AValue;
end;

procedure TPostScript.SetTitle(const AValue: String);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;

  // need to not hard-link these...
  FDocument[3] := '%%Title: '+AValue;
end;

procedure TPostScript.SetWidth(const AValue: Integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
  UpdateBoundingBox;
end;

{ Places the current canvas object into the document }
procedure TPostScript.GrabCanvas;
var
   I: Integer;
begin
  // internally calls this at the end of a page...

  I := 0;
  while I < FCanvas.PostScript.Count do begin
           Document.Add(FCanvas.PostScript[I]);
           I := I+1;
  end;
end;

{ Take our sizes and change the boundingbox line }
procedure TPostScript.UpdateBoundingBox;
begin
  // need to not hard-link this to line 1
  FDocument[1] := '%%BoundingBox: 0 0 '+IntToStr(FWidth)+' '+IntToStr(FHeight);
end;

{ Pattern Changed so update the postscript code }
procedure TPostScript.PatternChanged(Sender: TObject);
begin
  // called anytime a pattern changes.  Update the postscript code.
  // look for and delete the current postscript code for this pattern
  // then paste the pattern back into the code before the first page
  RemovePattern(Sender As TPSPattern);
  InsertPattern(Sender As TPSPattern);
end;

{ Places a pattern definition into the bottom of the header in postscript }
procedure TPostScript.InsertPattern(APattern: TPSPattern);
var
  I, J: Integer;
  MyStrings: TStringList;
begin
  I := 0;
  if FDocument.Count < 1 then begin
        // added pattern when no postscript exists - this shouldn't happen
        raise exception.create('Pattern inserted with no postscript existing');
        exit;
  end;
     
  for I := 0 to FDocument.count - 1 do begin
         if (FDocument[I] = '%%Page: 1 1') then begin
            // found it!
            // insert into just before that
            MyStrings := APattern.GetPS;
            for J := 0 to MyStrings.Count - 1 do begin
                FDocument.Insert(I-1+J, MyStrings[j]);
            end;
            exit;
         end;
  end;
end;

{Remove a pattern from the postscript code }
procedure TPostScript.RemovePattern(APattern: TPSPattern);
var
  I: Integer;
  MyName: String;
begin
  // this does NOT destroy the object, just removes from postscript
     
  if APattern.OldName <> '' then MyName := APattern.OldName
  else MyName := APattern.name;

  I := 0;
  if FDocument.Count < 1 then begin
        // added pattern when no postscript exists - this shouldn't happen
        raise exception.create('Pattern removed with no postscript existing');
        exit;
  end;
     
  for I := 0 to FDocument.Count - 1 do begin
         if (FDocument[I] = '%% PATTERN '+MyName) then begin
            // found it...
            // delete until gone
            while I < FDocument.Count - 1 do begin
                  // stay within our limites
                  if (FDocument[I] = '%% END PATTERN '+MyName) then begin
                     FDocument.Delete(I);
                     APattern.oldName := '';
                     exit;
                  end else FDocument.Delete(I);
            end;
         end;
  end;
end;

constructor TPostScript.Create;
begin
  inherited create;

  FDocument := TStringList.Create;

     // Set some defaults
  FHeight := 792; // 11 inches at 72 dpi
  FWidth := 612; // 8 1/2 inches at 72 dpi
  FCanvas := TPostScriptCanvas.Create(Self);

  FDocument.Clear;
  FDocument.Add('%!PS-Adobe-3.0');
  FDocument.Add('%%BoundingBox: 0 0 612 792');
  FDocument.Add('%%Creator: '+Application.ExeName);
  FDocument.Add('%%Title: '+FTitle);
  FDocument.Add('%%Pages: (atend)');
  FDocument.Add('%%PageOrder: Ascend');

     // Choose a standard font in case the user doesn't
  FDocument.Add('/AvantGarde-Book findfont');
  FDocument.Add('10 scalefont');
  FDocument.Add('setfont');

     // start our first page
  FPageNumber := 1;
  FDocument.Add('%%Page: 1 1'); // I'm still not sure why u put the page # twice
  FDocument.Add('newpath');

end;

destructor TPostScript.Destroy;
var
   I: Integer;
begin

  FCanvas.Free;
  FDocument.Free;
  
  // destroy the patterns
  if NumPatterns > 0 then begin
  for I := 0 to NuMPatterns-1 do begin
         Patterns[i].Free;
  end;
  end;
  
  // free the pattern pointer memory
  Reallocmem(Patterns, 0);
  
  inherited Destroy;

end;

{ Add a pattern to the array }
procedure TPostScript.AddPattern(APSPattern: TPSPattern);
begin
  // does NOT create the pattern, just insert in the array of patterns

  NumPatterns := NumPatterns+1;

  reallocmem(Patterns, sizeof(TPSPattern) * NumPatterns);
     
  Patterns[NumPatterns-1] := APSPattern;
end;

{ Find a pattern object by it's name }
function TPostScript.FindPattern(AName: String): TPSPattern;
var
   I: Integer;
begin
  Result := nil;
  if NumPatterns < 1 then exit;
  for I := 0 to NumPatterns-1 do begin
         if Patterns[I].Name = AName then begin
            result := Patterns[i];
            exit;
         end;
  end;
end;

function TPostScript.DelPattern(AName: String): Boolean;
begin
  {$IFNDEF DisableChecks}
  if AName<>'' then
    DebugLn('[TPostScript.DelPattern] ToDo ');
  {$ENDIF}

  // can't do that yet...
  Result:=false;
end;

{ Create a new pattern and inserts it into the array for safe keeping }
function TPostScript.NewPattern(AName: String): TPSPattern;
var
   MyPattern: TPSPattern;
begin
  MyPattern := TPSPattern.Create;
  AddPattern(MyPattern);
  MyPattern.Name := AName;
  MyPattern.OnChange := @PatternChanged;
  MyPattern.OldName := '';
     
     // Add this to the postscript now...

  InsertPattern(MyPattern);
  result := MyPattern;
end;

{ Start a new document }
procedure TPostScript.BeginDoc;
var
   I: Integer;
begin
  FCanvas.Clear;
  FDocument.Clear;
     
  // destroy the patterns
  if NumPatterns > 0 then
  begin
    for I := 0 to NuMPatterns-1 do
    begin
      Patterns[i].Free;
      Patterns[i]:=nil;
    end;
    NumPatterns:=0;
  end;

  // free the pattern pointer memory
  Reallocmem(Patterns, 0);

  FDocument.Add('%!PS-Adobe-3.0');
  FDocument.Add('%%BoundingBox: 0 0 612 792');
  FDocument.Add('%%Creator: '+Application.ExeName);
  FDocument.Add('%%Title: '+FTitle);
  FDocument.Add('%%Pages: (atend)');
  FDocument.Add('%%PageOrder: Ascend');

  // Choose a standard font in case the user doesn't
  FDocument.Add('/AvantGarde-Book findfont');
  FDocument.Add('10 scalefont');
  FDocument.Add('setfont');

  // start our first page
  FPageNumber := 1;
  FDocument.Add('%%Page: 1 1'); // I'm still not sure why u put the page # twice
  FDocument.Add('newpath');

  UpdateBoundingBox;
end;

{ Copy current page into the postscript and start a new one }
procedure TPostScript.NewPage;
begin
  // dump the current page into our postscript first
  GrabCanvas;

  // put end page definition...
  FDocument.Add('stroke');
  FDocument.Add('showpage');
  FPageNumber := FPageNumber+1;
  // start new page definition...
  FDocument.Add('%%Page: '+IntToStr(FPageNumber)+' '+IntToStr(FPageNumber));
  FDocument.Add('newpath');
  FCanvas.Clear;
end;

{ Finish off the document }
procedure TPostScript.EndDoc;
begin
     // dump the canvas into the postscript code
  GrabCanvas;

     // Start printing the document after closing out the pages
  FDocument.Add('stroke');
  FDocument.Add('showpage');
  FDocument.Add('%%Pages: '+IntToStr(FPageNumber));

     // okay, the postscript is all ready, so dump it to the text file
     // or to the printer
  FPageNumber := 0;
end;

{ TPSObject }

procedure TPSObject.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPSObject.Lock;
begin

end;

procedure TPSObject.UnLock;
begin

end;

{ TPSPen }

procedure TPSPen.SetPattern(const AValue: TPSPattern);
begin
  if FPattern=AValue then exit;
  FPattern:=AValue;
  Changed;
end;


procedure TPSPen.SetColor(Value: TColor);
begin
  FColor := Value;
  Changed;
end;

procedure TPSPen.Setwidth(value: Real);
begin
  FWidth := Value;
  Changed;
end;

constructor TPSPen.Create;
begin
  FPattern := nil;
end;

destructor TPSPen.Destroy;
begin
  // Do NOT free the pattern object from here...
  inherited Destroy;
end;

procedure TPSPen.Assign(Source: TPSPen);
begin
  if source = nil then exit;
     
  FWidth := Source.Width;
  FColor := Source.Color;
  FPattern := Source.Pattern;
end;

{ Return the pen definition as a postscript string }
function TPSPen.AsString: String;
var
   MyOut: String;
begin
  MyOut := '';

     // set all the features of this pen...
  if FPattern <> nil then begin
        // we have a pattern
        // uh... let's make it work for both colored and uncolored patterns
        // first for colored:

        if FPattern.PaintType = ptColored then
            MyOut := '/Pattern setcolorspace '+FPattern.Name+' setcolor '
        else begin
             // now for uncolored, use color from pen
             MyOut := '[/Pattern /DeviceRGB] setcolorspace '+IntToStr(GetRValue(FColor))+' '+IntToStr(GetGValue(FColor))+' '+
           IntToStr(GetBValue(FColor))+' '+FPattern.Name+' setcolor ';
        end;
           
  end else // no pattern do this:
          MyOut := IntToStr(GetRValue(FColor))+' '+IntToStr(GetGValue(FColor))+' '+
           IntToStr(GetBValue(FColor))+' setrgbcolor ';

  MyOut := MyOut + format('%f',[FWidth])+' setlinewidth ';
  Result := MyOut;
end;

{ TPSPattern }

{ Returns the pattern definition as postscript }
function TPSPattern.GetpostScript: TStringList;
var
   I: Integer;
begin
     // If nothing in the canvas, error
  if FCanvas.Postscript.Count < 1 then begin
        raise exception.create('Empty pattern');
        exit;
  end;
     
  FPostScript.Clear;
  With FPostScript do begin
          Add('%% PATTERN '+FName);
          Add('/'+FName+'proto 12 dict def '+FName+'proto begin');
          Add('/PatternType 1 def');
          case FPaintType of
               ptColored: Add('/PaintType 1 def');
               ptUncolored: Add('/PaintType 2 def');
          end;
          case FTilingType of
               ttConstant: Add('/TilingType 1 def');
               ttNoDistortion: Add('/TilingType 2 def');
               ttFast: Add('/TilingType 3 def');
          end;
          Add('/BBox ['+IntToStr(FBBox.Left)+' '+IntToStr(FBBox.Top)+' '+IntToStr(FBBox.Right)+' '+IntToStr(FBBox.Bottom)+'] def');
          Add('/XStep '+format('%f',[FXStep])+' def');
          Add('/YStep '+format('%f',[FYstep])+' def');
          Add('/PaintProc { begin');

          // insert the canvas
          for I := 0 to FCanvas.PostScript.Count - 1 do begin
              Add(FCanvas.PostScript[I]);
          end;

          // Add support for custom matrix later
          Add('end } def end '+FName+'proto [1 0 0 1 0 0] makepattern /'+FName+' exch def');
          Add('%% END PATTERN '+FName);
  end;
  Result := FPostScript;
end;

procedure TPSPattern.SetBBox(const AValue: TRect);
begin
  if FBBox=AValue then exit;
  FBBox:=AValue;
  //FCanvas.Width := FBBox.Right - FBBox.Left;
  FCanvas.Height := FBBox.Bottom - FBBox.Top;
  Changed;
end;

procedure TPSPattern.SetName(const AValue: String);
begin
  FOldName := FName;
  if FName=AValue then exit;
  FName:=AValue;
  Changed;
end;

procedure TPSPattern.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TPSPattern.SetPaintType(const AValue: TPSPaintType);
begin
  if FPaintType=AValue then exit;
  FPaintType:=AValue;
  Changed;
end;

procedure TPSPattern.SetTilingType(const AValue: TPSTileType);
begin
  if FTilingType=AValue then exit;
  FTilingType:=AValue;
  Changed;
end;

procedure TPSPattern.SetXStep(const AValue: Real);
begin
  if FXStep=AValue then exit;
  FXStep:=AValue;
  Changed;
end;

procedure TPSPattern.SetYStep(const AValue: Real);
begin
  if FYStep=AValue then exit;
  FYStep:=AValue;
  Changed;
end;

constructor TPSPattern.Create;
begin
  FPostScript := TStringList.Create;
  FPaintType := ptColored;
  FTilingType := ttConstant;
  FCanvas := TPostScriptCanvas.Create(nil);
  FName := 'Pattern1';
end;

destructor TPSPattern.Destroy;
begin
  FPostScript.Free;
  FCanvas.Free;
  inherited Destroy;
end;

end.
