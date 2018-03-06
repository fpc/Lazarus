{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Auxiliary classes aiding in the design of a report: 
    selection management, operations on selection.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportdesignobjectlist;

{$mode objfpc}{$H+}
{ $DEFINE DEBUGROL}

interface

uses
  Classes, SysUtils, graphics, lclintf, fpreportlclexport, fpreport, controls;

Const
  clResizeHandleSingle = clBlack;  // Pen color to draw selection resize handle
  clResizeHandleMulti = cldkGray;  // Pen color to draw selection resize handle, when multiselect

Type
  TSelectionSort = (ssNone,ssHorz,ssvert);
  THAlignAction  = (haNone,haLeft,haCenter,haRight,haSpace,haCentB);
  TVAlignAction  = (vaNone,vaTop,vaCenter,vaBottom,vaSpace,vaCentB);
  TSizeAdjust    = (saNone,saLargest,saSmallest,saValue);
  TFrameAction   = (faNone,faAll,faTop,faBottom,faLeft,faRight);
  TResizeHandlePosition = (rhNone,rhTopLeft,rhTop,rhTopRight,rhLeft,rhRight,rhBottomLeft, rhBottom,rhBottomRight);
  TResizeHandlePositions = set of TResizeHandlePosition;

  TFrameActions  = set of TFrameAction;

  // Indicates topmost object that was deleted
  TObjectDeleteResult = (odrNone,odrElement,odrBand,odrPage);
  TObjectSelection = (osAll,osCurrentSelected,osPreviousSelected,osPreviousOrCurrentlySelected);
  TGetObjectOption = (goBandHandle);
  TGetObjectOptions = Set of TGetObjectOption;

  { TReportObject }

  TReportObject = Class(TCollectionItem)
  private
    FElement: TFPReportElement;
    FSelected: Boolean;
    FPreviousSelected : Boolean;
    function GetAsBand: TFPReportCustomBand;
    function GetAsPage: TFPReportCustomPage;
    function GetIsBand: Boolean;
    function GetIsPage: Boolean;
    function GetIsPlainElement: Boolean;
    procedure SetElement(AValue: TFPReportElement);
    procedure SetSelected(AValue: Boolean);
  Protected
  Public
    Function MatchSelection (ObjectSelection : TObjectSelection) : Boolean;
    Property PreviousSelected : Boolean Read FPreviousSelected;
    Property AsPage : TFPReportCustomPage Read GetAsPage;
    Property AsBand : TFPReportCustomBand Read GetAsBand;
    Property Element : TFPReportElement Read FElement Write SetElement;
    Property Selected : Boolean Read FSelected Write SetSelected;
    Property IsBand : Boolean Read GetIsBand;
    Property IsPage : Boolean Read GetIsPage;
    Property IsPlainElement : Boolean Read GetIsPlainElement;
  end;

  TReportObjectArray = Array of TReportObject;

  { TReportObjectList }

  TReportObjectList = Class(TCollection)
  private
    FCanvasExport: TFPReportExportCanvas;
    FModified: Boolean;
    FOnReportChange: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FPage: TFPReportCustomPage;
    FPageOffSet: TPoint;
    FSelChangeCount : Integer;
    FLastSelectionBounds : TRect;
    FLastSelectionRect : TFPReportRect;
    FLastBandTextHeight : Integer;
    function DeleteElement(O: TReportObject): TObjectDeleteResult;
    function ElementToDeleteResult(E: TFPReportComponent): TObjectDeleteResult;
    function FindNextBand(ABand: TFPReportCustomBand): TFPReportCustomBand;
    function GetElement(AIndex : Integer): TFPReportElement;
    function GetObject(Aindex : Integer): TReportObject;
    procedure SetModified(AValue: Boolean);
  protected
    procedure MoveResizeRect(Var R: TRect; AOffset: TPoint; ApplyToPos: TResizeHandlePosition);
    function  GetSelectionArray(SelSort: TSelectionSort): TReportObjectArray;
    procedure SelectRectInvalid;
    Procedure SelectionChanged; virtual;
  Public
    // Do things
    Procedure LoadFromPage(APage : TFPReportCustomPage); virtual;
    Procedure ReportChanged; virtual;
    Procedure BeginSelectionUpdate;
    Procedure EndSelectionUpdate;
    Procedure ClearSelection;
    Procedure ClearPreviousSelection;
    procedure OrderBands(aBandTextheight, ADPI: Integer); virtual;
    procedure OrderBands(ACanvas: TCanvas; ADPI: Integer);
    Procedure DrawSelectionHandles;virtual;
    procedure DrawSelectionHandles(AOffset: TPoint; ApplyToPos : TResizeHandlePosition);virtual;
    function AddElement(AElement: TFPReportElement): TReportObject;virtual;
    function AddBand(ABand: TFPReportCustomBand): TReportObject;virtual;
    procedure AlignSelection(Hor: THAlignAction; Ver: TValignAction);virtual;
    Procedure MoveSelection(Delta : TPoint; ADPI : integer);virtual;
    procedure ResizeSelection(aHeight: TSizeAdjust; HSize: TFPReportUnits; aWidth: TSizeAdjust; WSize: TFPReportUnits);virtual;
    Procedure ResizeSelection(Delta : TPoint; ADPI : integer; ApplyToPos : TResizeHandlePosition);virtual;
    Procedure FrameSelection(aFrameAction : TFrameActions; doClearFirst : Boolean);
    Procedure ResetModified;
    Procedure SelectElement(E : TFPReportElement);
    // Will call selectionchanged, except when result=odrPage
    Function DeleteSelection : TObjectDeleteResult;
    // Various ways to get information
    Function HaveSelection : Boolean;
    Function IsMultiSelect : Boolean;
    function SelectionCount: Integer;
    function GetBandObjectAt(P: TPoint; AOptions : TGetObjectOptions): TReportObject;virtual;
    Function GetObjectAt(P : TPoint; AOptions : TGetObjectOptions) : TReportObject;virtual;
    function GetBandObjectsInRect(R : TRect; AOptions : TGetObjectOptions): TFPList;virtual;
    Function GetObjectsInRect(R : TRect; AOptions : TGetObjectOptions) : TFPList;virtual;
    Function BandHasSelection(B : TFPReportCustomBand; ObjectSelection : TObjectSelection) : Boolean;virtual;
    Function GetElementList(AClass : TClass) : TFPList;virtual;
    function GetSelectionBounds: TRect;virtual;
    function GetSelectionRect: TFPReportRect;virtual;
    Function HorizontalAlignOK(A: THAlignAction) : Boolean;virtual;
    Function VerticalAlignOK(A: TVAlignAction) : Boolean;virtual;
    function PointToResizeHandlePos(P: TPoint): TResizeHandlePosition;
    // Properties
    Property CanvasExport : TFPReportExportCanvas Read FCanvasExport Write FCanvasExport;
    Property OnSelectionChange : TNotifyEvent Read FOnSelectionChange Write FOnSelectionChange;
    Property OnReportChange : TNotifyEvent Read FOnReportChange Write FOnReportChange;
    Property Modified : Boolean Read FModified;
    Property Objects[Aindex : Integer] : TReportObject Read GetObject; default;
    Property Elements[AIndex : Integer] : TFPReportElement Read GetElement;
    Property Page : TFPReportCustomPage Read FPage;
    Property PageOffset : TPoint Read FPageOffSet Write FPageOffset;
  end;

  TReportDragDrop = Class(TDragObjectEx);

  { TMemoDragDrop }
  TMemoDragDropOption = (mddShowEditor,mddShowFormatting);
  TMemoDragDropOptions = Set of TMemoDragDropOption;

  TMemoDragDrop = Class(TReportDragDrop)
  private
    FContent: String;
    FOptions: TMemoDragDropOptions;
  Public
    Constructor Create(AControl : TControl; AContent : String; AOptions : TMemoDragDropOptions = []);
    Property Content : String Read FContent Write FContent;
    Property Options : TMemoDragDropOptions Read FOptions Write FOptions;
  end;

Function FindBandType(L :  TFPList; AClass : TClass; Var StartAt : Integer; ExtractResult : Boolean = False) : TFPReportCustomBand;
Function mmToPixels(Const Dist: TFPReportUnits; Const ADPI : Integer) : Integer;
Function PixelsToMM(Const Dist: Integer; Const ADPI : Integer) : TFPReportUnits;
Function PointToStr(P : TPoint) : String;
Function RectToStr(R : TRect) : String;

implementation

uses math;

Function PointToStr(P : TPoint) : String;

begin
  With P do
    Result:=Format('(%d,%d)',[X,Y]);
end;

Function RectToStr(R : TRect) : String;

begin
  With R Do
    Result:=Format('(%s x %s)',[PointToStr(R.TopLeft),PointToStr(R.BottomRight)])
end;

Function mmToPixels(Const Dist: TFPReportUnits; Const ADPI : Integer) : Integer;

begin
  Result:=Trunc(((Dist * ADPI)/cInchToMM)+0.5);
end;

Function PixelsToMM(Const Dist: Integer; Const ADPI : Integer) : TFPReportUnits;

begin
  Result:=(Dist*cInchToMM)/ADPI;
end;


Function FindBandType(L :  TFPList; AClass : TClass; Var StartAt : Integer; ExtractResult : Boolean = False) : TFPReportCustomBand;

Var
  I : Integer;

begin
  I:=StartAt;
  Result:=Nil;
  While (Result=Nil) and (I<L.Count) do
    begin
    if TObject(L[i]).InheritsFrom(AClass) then
      Result:=TObject(L[I]) as TFPReportCustomBand
    else
      Inc(i);
    end;
  If Result<>Nil then
    begin
    StartAt:=I;
    if ExtractResult then
      L.Delete(I);
    end;
end;

{ TMemoDragDrop }

constructor TMemoDragDrop.Create(AControl: TControl; AContent: String; AOptions: TMemoDragDropOptions);
begin
  Inherited Create(AControl);
  FContent:=AContent;
  FOptions:=AOptions;
end;

{ TReportObject }

procedure TReportObject.SetElement(AValue: TFPReportElement);
begin
  if FElement=AValue then Exit;
  FElement:=AValue;
end;

function TReportObject.GetIsBand: Boolean;
begin
  Result:=Element is TFPReportCustomBand;
end;

function TReportObject.GetIsPage: Boolean;
begin
  Result:=Element is TFPReportCustomPage;
end;

function TReportObject.GetIsPlainElement: Boolean;
begin
  Result:=Not (IsPage or IsBand);
end;

function TReportObject.GetAsBand: TFPReportCustomBand;
begin
  Result:=Element as TFPReportCustomBand;
end;

function TReportObject.GetAsPage: TFPReportCustomPage;
begin
  if IsPage then
    Result:=Element as TFPReportCustomPage
  else
    Result:=Nil;
end;

procedure TReportObject.SetSelected(AValue: Boolean);
begin
  if FSelected=AValue then Exit;
  FPreviousSelected:=FSelected;
  FSelected:=AValue;
  if Collection is TReportObjectList then
    (Collection as TReportObjectList).SelectionChanged;
end;

function TReportObject.MatchSelection(ObjectSelection: TObjectSelection): Boolean;

begin
  Case ObjectSelection of
    osAll : Result:=True;
    osCurrentSelected : Result:=Selected;
    osPreviousSelected :  Result:=PreviousSelected;
    osPreviousOrCurrentlySelected :  Result:=PreviousSelected or Selected;
  else
    Result:=False;
  end;
end;


{ TReportObjectList }

function TReportObjectList.GetElement(AIndex : Integer): TFPReportElement;
begin
  Result:=GetObject(AIndex).Element;
end;

function TReportObjectList.GetObject(Aindex : Integer): TReportObject;
begin
  Result:=Items[Aindex] as TReportObject;
end;

procedure TReportObjectList.SetModified(AValue: Boolean);
begin
  if FModified=AValue then Exit;
  FModified:=AValue;
end;

procedure TReportObjectList.SelectionChanged;
begin
  BeginSelectionUpdate;
  EndSelectionUpdate;
end;

procedure TReportObjectList.ReportChanged;
begin
  SetModified(True);
  if Assigned(OnReportChange) then
    OnReportChange(Self);
end;

procedure TReportObjectList.BeginSelectionUpdate;
begin
  Inc(FSelChangeCount);
end;

procedure TReportObjectList.SelectRectInvalid;

begin
  FLastSelectionBounds:=Default(TRect);
  FLastSelectionRect:=Default(TFPReportRect);
end;

procedure TReportObjectList.EndSelectionUpdate;
begin
  if FSelChangeCount<=0 then exit;
  Dec(FSelChangeCount);
  if (FSelChangeCount=0) then
    begin
    SelectRectInvalid;
    if Assigned(OnSelectionChange) then
      Begin
      OnSelectionChange(Self);
      end;
    end;
end;

function TReportObjectList.AddElement(AElement: TFPReportElement
  ): TReportObject;

Var
  C : TFPReportElementWithChildren;
  I : Integer;

begin
{$IFDEF DEBUGROL}Writeln('Adding  ',AElement.ClassName,' : ',AElement.Name);{$ENDIF}
  Result:=Add as TReportObject;
  Result.Element:=AElement;
  If AElement is TFPReportElementWithChildren then
    begin
    C:=AElement as TFPReportElementWithChildren;
    For I:=0 to C.ChildCount-1 do
      AddElement(C.Child[i]);
    end;
  ReportChanged;
end;

function TReportObjectList.AddBand(ABand: TFPReportCustomBand): TReportObject;

Var
  I : Integer;

begin
  Result:=Add as TReportObject;
  Result.Element:=ABand;
  For I:=0 to ABand.ChildCount-1 do
    AddElement(ABand.Child[i]);
  ReportChanged;
end;

procedure TReportObjectList.LoadFromPage(APage: TFPReportCustomPage);
begin
  Clear;
  AddElement(APage);
  FPage:=APage;
  ResetModified;
end;

procedure TReportObjectList.ClearSelection;

Var
  I : Integer;

begin
  BeginSelectionUpdate;
  try
    For I:=0 to Count-1 do
      GetObject(i).Selected:=False;
  finally
    EndSelectionUpdate;
  end;
end;

procedure TReportObjectList.ClearPreviousSelection;

Var
  I : Integer;

begin
  For I:=0 to Count-1 do
    GetObject(i).FPreviousSelected:=False;
end;

function TReportObjectList.FindNextBand(ABand: TFPReportCustomBand
  ): TFPReportCustomBand;

Var
  B : TFPReportCustomBand;
  I : integer;

begin
  Result:=Nil;
  For I:=0 to Count-1 do
    if Objects[i].IsBand then
      begin
      B:=Objects[i].AsBand;
      if B.Layout.Top>ABand.Layout.Top then
        if not (Assigned(Result) and (Result.Layout.Top>B.Layout.Top)) then
          Result:=B;
      end;
end;

procedure TReportObjectList.MoveSelection(Delta: TPoint; ADPI: integer);

Var
  I : Integer;
  It : TReportObject;
  ATop : TFPReportUnits;
  APrevBand,ANextBand : TFPReportCustomBand;
  RO : TReportObject;
  P : TPoint;

begin
  For I:=0 to Count-1 do
    begin
    IT:=GetObject(i);
    if It.Selected then
      if It.IsPlainElement then
        begin
        IT.Element.Layout.Left:= IT.Element.Layout.Left+PixelsToMM(Delta.X,ADPI);
        ATop:=IT.Element.Layout.Top+PixelsToMM(Delta.Y,ADPI);
        // Check if we must move to a next band.
        APrevBand:=IT.Element.Parent as TFPReportCustomBand;
{$IFDEF DEBUGROL} Writeln('PageOffset :',PointToStr(PageOffset));{$ENDIF}
        P.Y:=PageOffset.y+mmToPixels(FPage.Layout.Top+APrevBand.Layout.Top+IT.Element.Layout.Top,ADPI)+Delta.Y;
        P.X:=PageOffset.X+mmToPixels(FPage.Layout.Left+APrevBand.Layout.Left+IT.Element.Layout.Left,ADPI)+Delta.X;
        RO:=GetBandObjectAt(P,[goBandHandle]);
        if (RO<>Nil) and (RO.AsBand<>APrevBand) then
          begin
          {$IFDEF DEBUGROL}           Writeln('!!! Reparent detected !!!');{$ENDIF}
          ANextBand:=RO.AsBand;
          IT.Element.Parent:=ANextBand;
          {$IFDEF DEBUGROL}           Writeln('New parent: ',IT.Element.Parent.ClassName);{$ENDIF}
          // Correct Atop.
          ATop:=ATop-(ANextBand.Layout.Top-APrevBand.Layout.Top);
          IT.Element.Layout.Top:=ATop;
          end
        else
          IT.Element.Layout.Top:=ATop;
        end;
     end;
  SelectRectInvalid;
  ReportChanged;
end;

procedure TReportObjectList.ResizeSelection(aHeight: TSizeAdjust;
  HSize: TFPReportUnits; aWidth: TSizeAdjust; WSize: TFPReportUnits);

Var
  i : Longint;
  Arr : TReportObjectArray;
  S : TFPReportUnits;

begin
  if SelectionCount=0 then
    exit;
  Arr:=GetSelectionArray(ssNone);
  If (aHeight in [saSmallest,saLargest]) then
    begin
    HSize:=Arr[0].Element.Layout.Height;
    For I:=1 to Length(Arr)-1 do
      begin
      S:=Arr[I].Element.Layout.Height;
      Case aHeight of
        saSmallest : HSize:=Min(HSize,S);
        saLargest : HSize:=Max(HSize,S);
      end;
      end;
    end;
  If (aWidth in [saSmallest,saLargest]) then
    begin
    WSize:=Arr[0].Element.Layout.Width;
    For I:=1 to Length(Arr)-1 do
      begin
      S:=Arr[I].Element.Layout.Width;
      Case aWidth of
        saSmallest : WSize:=Min(WSize,S);
        saLargest : WSize:=Max(WSize,S);
      end;
      end;
    end;
  For I:=0 to Length(Arr)-1 do
    With Arr[i].Element.Layout do
      begin
      If (aHeight<>saNone) then
        Height:=HSize;
      If aWidth<>saNone then
        Width:=WSize;
      end;
  SelectRectInvalid;
  ReportChanged;
end;

procedure TReportObjectList.ResizeSelection(Delta: TPoint; ADPI: integer;
  ApplyToPos: TResizeHandlePosition);

Var
  I : Integer;
  It : TReportObject;
  RR : TFPReportRect;
  R : TRect;
//  P : TPoint;
  BH,BD : TFPReportUnits;
  BC : Boolean;

begin
  BC:=False;
  For I:=0 to Count-1 do
    begin
    IT:=GetObject(i);
    if It.Selected then
      if It.IsPlainElement then
        begin
        IT.Element.Layout.GetBoundsRect(RR);
        R.Left:=mmToPixels(RR.left,ADPI);
        R.Top:=mmToPixels(RR.Top,ADPI);
        R.Width:=mmToPixels(RR.Width,ADPI);
        R.Height:=mmToPixels(RR.Height,ADPI);
        MoveResizeRect(R,Delta,ApplytoPos);
        if R.Width<0 then
          R.Width:=ReSizeHandleWidth;
        if R.Height<0 then
          R.Height:=ReSizeHandleWidth;
        RR.Left:=PixelsToMM(R.left,ADPI);
        RR.Top:=PixelsToMM(R.Top,ADPI);
        RR.Width:=PixelsToMM(R.Width,ADPI);
        RR.Height:=PixelsToMM(R.Height,ADPI);
        IT.Element.Layout.SetPosition(RR);
        end
      else if IT.isBand then
        begin
        if ApplyToPos in [rhTopLeft,rhTopRight,rhTop,rhBottomLeft,rhBottomRight,rhBottom] then
          begin
          BH:=IT.asBand.Layout.Height;
          BD:=PixelsToMM(Delta.Y,aDPI);

          if ApplyToPos in [rhTopLeft,rhTopRight,rhTop] then
            BH:=BH-BD
          else
            BH:=BH+BD;
          if BH<0 then
            BH:=PixelsToMM(ReSizeHandleWidth,aDPI);
          IT.asBand.Layout.Height:=BH;
          BC:=True;
          end;
        end
     end;
  if BC then
    OrderBands(FLastBandTextHeight,aDPI);
  SelectRectInvalid;
  ReportChanged;
end;

procedure TReportObjectList.FrameSelection(aFrameAction: TFrameActions; doClearFirst: Boolean);

Const
  LineSets : Array[TFrameAction] of TFPReportFrameLines
     = ([],
        [flTop, flBottom, flLeft, flRight],
        [flTop],
        [flBottom],
        [flLeft],
        [flRight]);

Var
  I : Integer;
  A : TFrameAction;
  CS,S : TFPReportFrameLines;

begin
  S:=[];
  For A in TFrameAction do
    if A in aFrameAction then
      S:=S+LineSets[A];
  For I:=0 to Count-1 do
    If Objects[i].Selected then
      begin
      if doClearFirst then
        CS:=[]
      else
        CS:=Objects[i].Element.Frame.Lines;
      CS:=CS+S;
      Objects[i].Element.Frame.Lines:=CS;
      end;
  ReportChanged;
end;

procedure TReportObjectList.ResetModified;
begin
  FModified:=False;
end;

procedure TReportObjectList.SelectElement(E: TFPReportElement);

Var
  I : Integer;
  O : TReportObject;

begin
  For I:=0 to Count-1 do
    begin
    O:=Objects[i];
    O.Selected:=(O.Element=E);
    end;
end;

function TReportObjectList.DeleteElement(O: TReportObject): TObjectDeleteResult;

Var
  R : TObjectDeleteResult;
  I : integer;

begin
  Result:=ElementToDeleteResult(O.Element);
  // Delete children first
  if (O.Element is TFPReportElementWithChildren) then
    begin
    I:=Count-1;
    While I>=0 do
      begin
      if (Objects[i].Element.Parent=O.Element) then
        begin
        R:=DeleteElement(Objects[i]);
        // Normally not possible.
        If R>Result then
          Result:=R;
        end;
      Dec(I);
      // Deleting children can cause the deletion of a lot of items.
      if I>=Count then
        I:=Count-1;
      end;
    end;
  // Delete element
  O.Element.Free;
  O.FElement:=Nil;
  // Delete object in list
  O.Free;
end;

function TReportObjectList.ElementToDeleteResult(E : TFPReportComponent): TObjectDeleteResult;

begin
  if E is TFPReportCustomPage then
    result:=odrPage
  else if E is TFPReportCustomBand then
    result:=odrBand
  else if E is TFPReportElement then
    result:=odrElement
  else
    result:=odrNone
end;

function TReportObjectList.DeleteSelection: TObjectDeleteResult;

Var
  R : TObjectDeleteResult;
  I : integer;

begin
  Result:=odrNone;
  I:=Count-1;
  While (I>=0) do
    begin
    if (Objects[i].Selected)  then
      begin
      R:=DeleteElement(Objects[i]);
      if R>Result then Result:=R;
      end;
    Dec(I);
    if I>=Count then
      I:=Count-1;
    end;
  ReportChanged;
  if (Result<>odrPage) then
    SelectionChanged;
end;

function TReportObjectList.HaveSelection: Boolean;
Var
  I : Integer;

begin
  Result:=False;
  I:=0;
  While (Not Result) and (I<Count) do
    begin
    Result:=Objects[i].Selected;
    Inc(I);
    end;
end;

function TReportObjectList.SelectionCount : Integer;

Var
  I : Integer;

begin
  Result:=0;
  For I:=0 to Count-1 do
    If Objects[i].Selected then
      Inc(Result);
end;

function TReportObjectList.GetBandObjectAt(P: TPoint; AOptions : TGetObjectOptions): TReportObject;

Var
  I : Integer;
  R : TRect;
  {$IFDEF DEBUGROL}  N : String;{$ENDIF}

begin
{$IFDEF DEBUGROL}Writeln('GetBandObjectAt(',P.X,',',P.Y,')');{$ENDIF}
  Result:=Nil;
  I:=0;
  While (Result=Nil) and (I<COunt) do
    begin
    Result:=Objects[i];
    if Result.IsBand then
      begin
{$IFDEF DEBUGROL}        N:=Result.Element.ClassName;{$endif}
      R:=FCanvasExport.GetBandRect(Result.AsBand,goBandHandle in Aoptions);
      if Not PtInRect(R,P) then
        Result:=Nil;
{$IFDEF DEBUGROL}Writeln(PointToStr(P),' in Band[',N,'] : ',RectToStr(R),' : ',Assigned(Result));{$ENDIF}
      end
    else
      Result:=Nil;
    Inc(I);
    end;
{$IFDEF DEBUGROL}
  if Result<>Nil then
    Writeln('GetBandObjectAt(',P.X,',',P.Y,') : ',Result.Element.ClassName,' (',Result.Element.Name,')')
  else
    Writeln('GetBandObjectAt(',P.X,',',P.Y,') : Nil');
{$ENDIF}
end;


function TReportObjectList.GetObjectAt(P: TPoint; AOptions : TGetObjectOptions): TReportObject;

Var
  B,O : TReportObject;
  RB : TFPReportCustomBand;
  I : Integer;
  R : TRect;

begin
  {$IFDEF DEBUGROL}Writeln('GetObjectAt(',PointToStr(P),')');{$ENDIF}
  Result:=Nil;
  if Objects[0].IsPage then
    begin
    if not PtInRect(FCanvasExport.GetPageRect(Objects[0].AsPage,True),P) then
      exit;
    end;
  Result:=Objects[0];
  B:=GetBandObjectAt(P,AOptions);
  if B=Nil then // Careful, assumes that no printable is outside a band...
    Exit;
  Result:=B;
  RB:=B.AsBand;
  O:=Nil;
  I:=0;
  While (O=Nil) and (I<COunt) do
    begin
    O:=Objects[i];
    {$IFDEF DEBUGROL}
    if O.IsPLainElement then
      Writeln(PointToStr(P),': examining  element[',O.Element.ClassName,'] : parent OK ',O.Element.Parent=RB);
    {$ENDIF}
    if Not (O.IsPlainElement and (O.Element.Parent=RB)) then
      O:=Nil
    else
      begin
      R:=FCanvasExport.GetElementRect(B.AsBand,O.Element);
      {$IFDEF DEBUGROL}Writeln(PointToStr(P),' in element[',O.Element.ClassName,'] : ',RectToStr(R),' : ',PtInRect(R,P));{$ENDIF}
      if not PtInRect(R,P) then
        O:=Nil;
      end;
    Inc(I);
    end;
  if O<>Nil then
    Result:=O;
{$IFDEF DEBUGROL}
  if Result<>Nil then
    Writeln('GetObjectAt(',PointToStr(P),') : ',Result.Element.ClassName,' (',Result.Element.Name,')')
  else
    Writeln('GetObjectAt(',PointToStr(P),') : Nil');
{$ENDIF}
end;

function TReportObjectList.GetBandObjectsInRect(R: TRect; AOptions: TGetObjectOptions): TFPList;

Var
  I : Integer;
  BR,D : TRect;
  O : TReportObject;

begin
  Result:=TFPList.Create;
  try
    For I:=0 to Count-1 do
      begin
      O:=Objects[i];
      if O.IsBand then
        begin
        BR:=FCanvasExport.GetBandRect(O.AsBand,goBandHandle in AOptions);
        if IntersectRect(D,BR,R) then
          Result.Add(O.AsBand);
        end
      end;
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

function TReportObjectList.GetObjectsInRect(R: TRect; AOptions: TGetObjectOptions): TFPList;

Var
  I,OI : Integer;
  D,ER : TRect;
  BL : TFPList;
  O : TReportObject;
  B : Boolean;

begin
  BL:=Nil;
  Result:=TFPList.Create;
  try
    try
      BL:=GetBandObjectsInRect(R,AOptions);
      {$IFDEF DEBUGROL}
      For I:=0 to BL.Count-1 do
        Writeln('Found band ',TReportObject(BL[i]).ClassName);
      {$ENDIF}
      For I:=0 to Count-1 do
        begin
        O:=Objects[i];
        OI:=BL.IndexOf(O.Element.Parent);
        if Not (O.IsPlainElement and (OI<>-1)) then
          O:=Nil
        else
          begin
          ER:=FCanvasExport.GetElementRect(O.Element.Parent as TFPReportCustomBand,O.Element);
          B:=IntersectRect(D,ER,R);
          {$IFDEF DEBUGROL}Writeln(RectToStr(R),' in element[',O.Element.ClassName,'] : ',RectToStr(ER),' : ',B);{$ENDIF}
          if not B then
            O:=Nil;
          end;
        if O<>Nil then
          Result.Add(O);
        end;
    except
      FreeAndNil(Result);
      Raise;
    end;
  finally
    BL.Free;
  end;

end;

function TReportObjectList.BandHasSelection(B: TFPReportCustomBand;
  ObjectSelection: TObjectSelection): Boolean;

Var
  I : Integer;
  O : TReportObject;

begin
  Result:=False;
  I:=0;
  While (Not Result) and (I<Count) do
    begin
    O:=Objects[i];
    Result:=(O.Element.Parent=B) and O.MatchSelection(ObjectSelection);
    Inc(I);
    end;
end;

function TReportObjectList.IsMultiSelect: Boolean;

Var
  Selcount,I : Integer;

begin
  SelCount:=0;
  I:=0;
  While (SelCount<2) and (I<Count) do
    begin
    If Objects[i].Selected then
      Inc(SelCount);
    Inc(I);
    end;
  Result:=SelCount>1;
end;

Type
  TMyBand = Class(TFPReportCustomBand);
  TMyDataBand = Class(TFPReportCustomDataBand);


procedure TReportObjectList.OrderBands(ACanvas : TCanvas; ADPI : Integer);

begin
  OrderBands(ACanvas.TextHeight('W'),aDPI);
end;

procedure TReportObjectList.OrderBands(aBandTextheight, ADPI : Integer);

Var
  L : TFPList;
  DY,Y : TFPReportUnits;

  // Position band and remove it from the list. Recurses to add child bands.
  Function AddBandToList(ABand : TFPReportCustomBand) : Boolean;

  begin
    Result:=Assigned(ABand);
    If not Result then
      exit;
{$IFDEF DEBUGROL}Writeln('Placing band ',ABand.ClassName,'(',ABAnd.Name,') at ',Y);{$ENDIF}
    ABand.Layout.Top:=Y;
    Y:=Y+DY+ABand.Layout.Height;
    L.Remove(Aband);
    // Recurse
    AddBandToList(TFPReportCustomBand(TMyBand(Aband).ChildBand));
  end;

  // Find a band of given type, if it exists add it (remove it from the list)

  Function MaybeAddBand(AClass : TClass) : TFPReportCustomBand;

  Var
    I : Integer;

  begin
    I:=0;
    Result:=FindBandType(L,AClass,I);
    AddBandToList(Result);
  end;

  // Add all bands of a given class, with the same data loop.
  Procedure AddSameDataLoopBands(ADetail : TFPReportCustomDataBand; AClass : TFPReportCustomBandWithDataClass);

  var
    i : integer;
  begin
    I:=0;
    While (I<L.Count) do
      begin
      if TObject(L[i]) is AClass then
        if (TFPReportCustomDataBand(L[i]).Data=ADetail.Data) then
          begin
          AddBandToList(TFPReportCustomDataBand(L[i]));
          I:=-1;
          end;
      Inc(I);
      end;
  end;

  Function AddBandsForMaster(ADetail : TFPReportCustomDataBand; AMaster :TFPReportCustomDataBand) : Boolean;

  Var
    i : integer;
    M : TMyDataBand;

  begin
     M:=TMyDataBand(ADetail);
     Result:=M.MasterBand=AMaster;
     if not Result then
       exit;
     // Add band header
     AddBandToList(M.HeaderBand);
     // Add group headers
     AddSameDataLoopBands(M,TFPReportCustomGroupHeaderBand);
     // Add band
     AddBandToList(M);
     // Detail bands, if any
     I:=0;
     While (I<L.Count) do
       begin
       if TObject(L[i]) is TFPReportCustomDataBand then
         if AddbandsForMaster(TFPReportCustomDataBand(L[i]),ADetail) then
           I:=-1; // Reset loop
       Inc(I);
       end;
     // Add group footers
     AddSameDataLoopBands(M,TFPReportCustomGroupFooterBand);
     // Add footer band
     AddBandTOList(M.FooterBand);
  end;

Var
  TH, I : Integer;
  F : TFPReportCustomBand;

begin
  // Start position
  TH:=ABandTextHeight+(BandTitleMargin * 2);
  Y:=PixelsToMM(TH,ADPI);
{$IFDEF DEBUGROL}Writeln('Textheight : ',TH,' translates to Y: ',Y,' (back to  pixels: ',mmToPixels(Y,ADPI),')');{$ENDIF}
  // Delta
  TH:=ABandTextHeight+(BandTitleMargin * 2)+BandTitleOffset;
  DY:=PixelsToMM(TH,ADPI);
{$IFDEF DEBUGROL}Writeln('Textheight : ',TH,' translates to DY: ',DY,' (back to  pixels: ',mmToPixels(DY,ADPI),')');{$ENDIF}
  FLastBandTextheight:=ABandTextHeight;
  L:=GetElementList(TFPReportCustomBand);
  try
    MaybeAddBand(TFPReportCustomPageHeaderBand);
    MaybeAddBand(TFPReportCustomTitleBand);
    MaybeAddBand(TFPReportCustomColumnHeaderBand);
    I:=0;
    While (I<L.Count) do
      begin
      if TObject(L[i]) is TFPReportCustomDataBand then
        if AddbandsForMaster(TFPReportCustomDataBand(L[i]),Nil) then
          I:=-1; // Reset loop
      Inc(I);
      end;
    MaybeAddBand(TFPReportCustomColumnFooterBand);
    MaybeAddBand(TFPReportCustomSummaryBand);
    // Extract
    I:=0;
    F:=FindBandType(L,TFPReportCustomPageFooterBand,I,True);
    While L.Count>0 do
      AddBandToList(TFPReportCustomBand(L[0]));
    AddBandToList(F);
  finally
    L.Free;
  end;
end;

procedure TReportObjectList.DrawSelectionHandles;

begin
  DrawSelectionHandles(Point(0,0),rhNone);
end;

procedure TReportObjectList.MoveResizeRect(Var R : TRect; AOffset : TPoint; ApplyToPos : TResizeHandlePosition);

Const
  PosNeedsXOffset = [rhNone,rhTopLeft,rhLeft,rhBottomLeft];
  PosNeedsYOffset = [rhNone,rhTopLeft,rhTop,rhTopRight];

  PosNeedsXResize = [rhTopLeft,rhBottomLeft,rhTopRight,rhBottomRight,rhRight,rhLeft];
  PosNeedsYResize = [rhTopLeft,rhBottomLeft,rhTopRight,rhBottomRight,rhTop,rhBottom];

  Function CalcOff(Dist: Integer; WhenIn : TResizeHandlePositions) : integer;
  begin
    Result:=Dist*Ord(ApplytoPos in WhenIn);
  end;

  Function CalcSize(aCurrent,aMove,aDelta: Integer) : integer;
  begin
    if aMove=0 then
      Result:=aCurrent+aDelta
    else
      Result:=aCurrent-Amove
  end;

Var
  XO,YO : Integer;

begin
  XO:=CalcOff(AOffset.X,PosNeedsXOffset);
  YO:=CalcOff(AOffset.Y,PosNeedsYOffset);
  OffsetRect(R,XO,YO);
  if (ApplyToPos in PosNeedsXResize) then
    R.width:=CalcSize(R.Width,XO,AOffset.X);
  if (ApplyToPos in PosNeedsYResize) then
    R.Height:=CalcSize(R.Height,YO,AOffset.Y);
end;

procedure TReportObjectList.DrawSelectionHandles(AOffset : TPoint; ApplyToPos : TResizeHandlePosition);

Var
  R : TRect;
  C : TColor;
  MW,MH : Integer;

begin
  R:=GetSelectionBounds;
  MoveResizeRect(R,aOffset,ApplyToPos);
  FCanvasExport.DrawSelectionRect(R);
  If IsMultiSelect then
    C:=clResizeHandleMulti
  else
    C:=clResizeHandleSingle;
  FCanvasExport.DrawSelectionHandle(R.TopLeft,C);
  FCanvasExport.DrawSelectionHandle(Point(R.Right,R.Top),C);
  FCanvasExport.DrawSelectionHandle(Point(R.Left,R.Bottom),C);
  FCanvasExport.DrawSelectionHandle(R.BottomRight,C);
  MW:=(R.Right+R.Left) div 2;
  MH:=(R.Top+R.Bottom) div 2;
  FCanvasExport.DrawSelectionHandle(Point(MW,R.Top),C);
  FCanvasExport.DrawSelectionHandle(Point(MW,R.Bottom),C);
  FCanvasExport.DrawSelectionHandle(Point(R.Left,MH),C);
  FCanvasExport.DrawSelectionHandle(Point(R.Right,MH),C);
end;

function TReportObjectList.GetElementList(AClass: TClass): TFPList;

Var
  I : Integer;
  L : TFPList;

begin
  Result:=nil;
  L:=TFPList.Create;
  try
    for I:=0 to Count-1 do
      If Elements[i] is AClass then
        L.Add(Elements[i]);
    Result:=L;
    L:=Nil;
  finally
    L.Free;
  end;
end;


function TReportObjectList.GetSelectionBounds: TRect;

Var
  D,E : TRect;
  I : Integer;
  El,P : TFPReportElement;
  APage : TFPReportCustomPage;

begin
  If Not IsRectEmpty(FLastSelectionBounds) then
    Exit(FLastSelectionBounds);
  D.Bottom:=-Maxint;
  D.Right:=-Maxint;
  D.Left:=MaxInt;
  D.Top:=MaxInt;
  for I:=0 to Count-1 do
    if Objects[i].Selected then
      begin
      El:=Elements[i];
{$IFDEF DEBUGROL}Writeln('Examining selection: ',El.ClassName);{$ENDIF}
      if el is TFPReportCustomBand then
        E:=FCanvasExport.GetBandRect(EL as TFPReportCustomBand,False)
      else if el is TFPReportCustomPage then
        begin
        APage:=el as TFPReportCustomPage;
        E:=FCanvasExport.GetPageRect(APage);
        OffsetRect(E,-FCanvasExport.HmmToPixels(APage.Margins.Left),
                     -FCanvasExport.VmmToPixels(APage.Margins.Top)
                     );
        end
      else
        begin
        P:=EL.Parent;
        While (P<>Nil) and not (P is TFPReportCustomBand) do
          P:=P.Parent;
        If P=Nil then
          continue;
        E:=FCanvasExport.GetElementRect(P as TFPReportCustomBand,El);
        end;
      D.Bottom:=Max(E.Bottom,D.Bottom);
      D.Right:=Max(E.Right,D.Right);
      D.Left:=Min(E.Left,D.Left);
      D.Top:=Min(E.Top,D.Top);
      end;
  FLastSelectionBounds:=D;
  Result:=D;
end;

function TReportObjectList.GetSelectionRect: TFPReportRect;

Var
  L,R,T,B : TFPReportUnits;
  I : Integer;
  El : TFPReportElement;
  ER : TFPReportRect;

begin
  if not FLastSelectionRect.IsEmpty then
    Exit(FLastSelectionRect);
  L:=Maxint;
  R:=-Maxint;
  T:=Maxint;
  B:=-Maxint;
  for I:=0 to Count-1 do
    if Objects[i].Selected then
      begin
      El:=Elements[i];
{$IFDEF DEBUGROL}Writeln('Examining selection: ',El.ClassName);{$ENDIF}
      if el is TFPReportElement then
        begin
        EL.Layout.GetBoundsRect(ER);
        L:=Min(ER.Left,L);
        R:=Max(ER.Right,R);
        T:=Min(ER.Top,T);
        B:=Max(ER.Bottom,B);
        end;
      end;
  FLastSelectionRect.SetRect(L,T,R-L,B-T);
  Result:=FLastSelectionRect;
  {$IFDEF DEBUGROL}   Writeln('Selection rect ',Result.AsString);{$ENDIF}
end;

function TReportObjectList.HorizontalAlignOK(A: THAlignAction): Boolean;

Var
  I : Integer;
  O : TReportobject;

begin
  I:=0;
  Result:=True;
  if (A=haNone) then
    exit;
  While Result and (I<Count) do
    begin
    O:=Objects[i];
    if O.Selected then
      begin
      Result:=O.IsPlainElement;
      end;
    Inc(I);
    end;
end;

function TReportObjectList.VerticalAlignOK(A: TVAlignAction): Boolean;

Var
  I : Integer;
  O : TReportobject;
  P : TFPReportElement;

begin

  I:=0;
  P:=Nil;
  Result:=True;
  if (A=vaNone) then
    exit;
  While Result and (I<Count) do
    begin
    O:=Objects[i];
    if O.Selected then
      begin
      Result:=O.IsPlainElement;
      // For vertical alignment, all elements must be on the same band!
      if Result then
        if P=Nil then
          P:=O.Element.Parent
        else
          Result:=P=O.Element.Parent
      end;
    Inc(I);
    end;
end;

function TReportObjectList.PointToResizeHandlePos(P: TPoint): TResizeHandlePosition;

Type
  TAxisPos = (apNone,apLow,apMiddle,apHigh);
  TAxisHandlePosArray =  Array[TAxisPos] of TResizeHandlePosition;

Const
  TopPositions    : TAxisHandlePosArray = (rhNone,rhTopLeft,rhTop,rhTopRight);
  BottomPositions : TAxisHandlePosArray = (rhNone,rhbottomLeft,rhBottom,rhBottomRight);
  CenterPositions : TAxisHandlePosArray = (rhNone,rhLeft,rhNone,rhRight);

  Function getAxisPos(aPos, aLow,aHigh : Integer) : TAxisPos;

  begin
    Result:=apNone;
    if (aPos<ALow) or (aPos>AHigh) then
       exit;
    if (aPos<=(aLow+ReSizeHandleWidth)) then
      result:=apLow
    else if (aPos>=(aHigh-ReSizeHandleWidth)) then
      result:=apHigh
    else
      begin
      aPos:=aPos-((aLow+aHigh) div 2);
      if Abs(aPos)<ReSizeHandleHalfWidth then
        Result:=apMiddle
      end;
  end;

Var
  R : TRect;
  xPos,yPos : TAxisPos;

begin
  Result:=rhNone;
  R:=GetSelectionBounds;
  InflateRect(R,ReSizeHandleHalfWidth,ReSizeHandleHalfWidth);
  XPos:=getAxisPos(P.x,R.Left,R.Right);
  YPos:=getAxisPos(P.Y,R.Top,R.Bottom);
{$IFDEF DEBUGROL}Writeln(PointToStr(P),' : ',XPos,',',YPos);{$ENDIF}
  if xPos<>apNone then
    Case YPos of
      apLow    : Result:=TopPositions[xPos];
      apHigh   : Result:=BottomPositions[xPos];
      apMiddle : Result:=CenterPositions[xPos];
      // Else not needed
    end;
end;

Function HCompare (P1,P2 : Pointer) : Integer;

Var
  L1,L2: TFPReportLayout;

begin
  L1:=TReportObject(P1).Element.Layout;
  L2:=TReportObject(P2).Element.Layout;
  Result:=Trunc((L1.Left + (L1.Width / 2)) - (L2.Left + (L2.Width / 2)))
end;

Function VCompare (P1,P2 : Pointer) : Integer;

Var
  L1,L2: TFPReportLayout;

begin
  L1:=TReportObject(P1).Element.Layout;
  L2:=TReportObject(P2).Element.Layout;
  Result:=Trunc((L1.Top + (L1.Height / 2)) - (L2.Top + (L2.Height / 2)))
{ Used in sorting vertically. }
end;

// Get array of selected objects, optionally sorted according to horz/vert left/top coordinate
function TReportObjectList.GetSelectionArray(SelSort: TSelectionSort
  ): TReportObjectArray;

Var
  I : Integer;
  L: TFPList;

begin
  L:=TFPList.Create;
  try
    L.Capacity:=SelectionCount;
    For I:=0 to Count-1 do
      if Objects[i].Selected then
        L.Add(Objects[I]);
    Case SelSort of
      ssVert: L.Sort(@VCompare);
      ssHorz: L.Sort(@HCompare);
    end;
    SetLength(Result,L.Count);
    For I:=0 to Length(Result)-1 do
      Result[I]:=TReportObject(L[i]);
  finally
    L.Free;
  end;
end;

procedure TReportObjectList.AlignSelection(Hor: THAlignAction;
  Ver: TValignAction);


Var
  HCenter,VCenter : TFPReportUnits;
  HSCenter,VSCenter,Delta : TFPReportUnits;
  OutlineRect : TFPReportRect;

  Procedure AlignControl (El : TFPReportElement; Hor : THAlignAction; Ver : TValignAction; IsBorder : Boolean);

  Var
    NewRect : TFPReportRect;
    BHCenter,BVCenter : TFPReportUnits;
    HOffset,VOffset : TFPReportUnits;

  begin
    BHCenter:=0;
    BVCenter:=0;
    El.Layout.GetBoundsRect(NewRect);
    if Assigned(EL.Parent) then
      With EL.Parent.Layout Do
        begin
        BHCenter:=Width / 2;
        BVCenter:=Height / 2;
        end;
    HOffset:=0;
    VOffset:=0;
    Case hor of
      haleft   : HOffset:=OutLineRect.Left-NewRect.Left;
      haRight  : HOffset:=OutLineRect.Right-NewRect.Right;
      haCenter : HOffset:=HCenter-(Newrect.Right+NewRect.Left) / 2;
      haCentB  : HOffset:=BHCenter-(NewRect.Right+NewRect.Left) / 2;
      haSpace  : If Not IsBorder Then
                   HOffset:=HSCenter-(Newrect.Right+NewRect.Left) / 2;
    end;
    Case Ver of
      vaTop    : VOffset:=OutLineRect.Top-NewRect.Top;
      vaBottom : VOffset:=OutLineRect.Bottom-NewRect.Bottom;
      vaCenter : VOffset:=VCenter-(Newrect.Bottom+NewRect.Top) / 2;
      vaCentB  : VOffSet:=BVCenter-(NewRect.Bottom+NewRect.Top) / 2;
      vaSpace  : If Not IsBorder Then
                   VOffset:=VSCenter-(Newrect.Bottom+NewRect.Top) / 2;
    end;
    // Go back Relative to the band..
    NewRect.OffsetRect(HOffset,VOffset);
    EL.Layout.Left:=NewRect.Left;
    EL.Layout.Top:=NewRect.Top;
  end;




var
  I : longint;
  TempHor : THAlignAction;
  TempVer : TVAlignAction;
  Arr : TReportObjectArray;
  L : TFPReportLayout;

begin
  OutlineRect:=GetSelectionRect;
  HCenter:=(OutlineRect.Right+OutlineRect.Left) / 2;
  VCenter:=(OutlineRect.Top+OutlineRect.Bottom) / 2;
  If (Hor<>haSpace) and (Ver<>vaSpace) then
    begin
    Arr:=GetSelectionArray(ssNone);
    For I:=0 to Length(Arr)-1 do
      AlignControl(Arr[i].element,Hor,Ver,(I=0) or (I=Length(Arr)-1));
    end
  else
    begin
    // When spacing, we get an array sorted on position.
    // We then do a run and apply a delta to the position to obtain the new position.
    // We must do two runs, because the sort is different for
    // Horizontal and vertical spacing!
    // Horizontal run
    If (Hor=haSpace) then
      begin
      Arr:=GetSelectionArray(ssHorz);
      L:=TReportObject(Arr[0]).Element.Layout;
      HSCenter:=L.Left+(L.Width/2);
      If Length(Arr)<=1 then
        Delta:=0
      else
        begin
        L:=TReportObject(Arr[Length(Arr)-1]).Element.Layout;
        Delta:=((L.Left +L.Width/2) - HSCenter) / (Count-1);
        end;
      If Ver=vaSpace then
        TempVer:=vaNone
      else
        TempVer:=Ver;
      For I:=0 To Length(Arr)-1 do
        begin
        AlignControl(Arr[i].Element,haSpace,TempVer,(I=0) or (I=Length(Arr)-1));
        // HCenter becomes to what we must align.
        HSCenter:=HSCenter+Delta;
        end;
      end;
    // Vertical run.
    If (Ver=VaSpace) then
      begin
      Arr:=GetSelectionArray(ssVert);
      L:=TReportObject(Arr[0]).Element.Layout;
      VSCenter:=L.Top+(L.Height/2);
      If Length(Arr)<=1 then
        Delta:=0
      else
        begin
        L:=TReportObject(Arr[Length(Arr)-1]).Element.Layout;
        Delta:=((L.Top +L.Height/2) - VSCenter) / (Count-1);
        end;
      If Hor=haSpace then
        TempHor:=haNone
      else
        TempHor:=Hor;
      For I:=0 To Length(Arr)-1 do
        begin
        AlignControl(Arr[i].Element,TempHor,vaSpace,(I=0) or (I=Length(Arr)-1));
        // VSCenter becomes to what we must align.
        VSCenter:=VSCenter+Delta;
        end;
      end;
    end;
  ReportChanged;
end;

end.

