unit fpguithemes;

{$mode objfpc}{$H+}

interface

uses
  // rtl
  Classes, SysUtils,
  // lcl
  Controls, Graphics, Themes, LCLType, LazUTF8, Types,
  // fpgui
  fpguiobjects, fpg_base, fpg_main, fpguiproc
  ;

{.$DEFINE FPGUIDEBUGTHEMES}

type

  { TFPGUIThemeServices }

  TFPGUIThemeServices = class(TThemeServices)
  private
  protected
    //function GetTheme(Element: TThemedElement): HTHEME;
    function InitThemes: Boolean; override;
    function UseThemes: Boolean; override;
    function ThemedControlsEnabled: Boolean; override;
    
    procedure InternalDrawParentBackground(Window: HWND; Target: HDC; Bounds: PRect); override;
  public
    destructor Destroy; override;
    function GetDetailSize(Details: TThemedElementDetails): TSize; override;
    (*
    function GetDetailRegion(DC: HDC; Details: TThemedElementDetails; const R: TRect): HRGN; override;
    function GetStockImage(StockID: LongInt; out Image, Mask: HBitmap): Boolean; override;
    function GetOption(AOption: TThemeOption): Integer; override;
    function GetTextExtent(DC: HDC; Details: TThemedElementDetails; const S: String; Flags: Cardinal; BoundingRect: PRect): TRect; override;
    *)
    procedure DrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect = nil); override;
    (*
    procedure DrawEdge(DC: HDC; Details: TThemedElementDetails; const R: TRect; Edge, Flags: Cardinal;
      AContentRect: PRect = nil); override;
    procedure DrawIcon(DC: HDC; Details: TThemedElementDetails; const R: TRect;
      himl: HIMAGELIST; Index: Integer); override;
    procedure DrawIcon(ACanvas: TPersistent; Details: TThemedElementDetails;
      const P: TPoint; AImageList: TPersistent; Index: Integer); override;
    *)
    (*
    procedure DrawText(DC: HDC; Details: TThemedElementDetails;
      const S: String; R: TRect; Flags, Flags2: Cardinal); override;
    procedure DrawText(ACanvas: TPersistent; Details: TThemedElementDetails;
      const S: String; R: TRect; Flags, Flags2: Cardinal); override;

    procedure DrawTextEx(DC: HDC; Details: TThemedElementDetails;
      const S: String; R: TRect; Flags: Cardinal; Options: PDTTOpts);
    *)
    function ContentRect(DC: HDC; Details: TThemedElementDetails; BoundingRect: TRect): TRect; override;
    (*
    function HasTransparentParts(Details: TThemedElementDetails): Boolean; override;
    procedure PaintBorder(Control: TObject; EraseLRCorner: Boolean); override;
    *)
  end;

implementation

uses
  TmSchema;

const
  ThemeDataNames: array[TThemedElement] of pchar = (
    'button',      // teButton
    'clock',       // teClock
    'combobox',    // teComboBox
    'edit',        // teEdit
    'explorerbar', // teExplorerBar
    'header',      // teHeader
    'listview',    // teListView
    'menu',        // teMenu
    'page',        // tePage
    'progress',    // teProgress
    'rebar',       // teRebar
    'scrollbar',   // teScrollBar
    'spin',        // teSpin
    'startpanel',  // teStartPanel
    'status',      // teStatus
    'tab',         // teTab
    'taskband',    // teTaskBand
    'taskbar',     // teTaskBar
    'toolbar',     // teToolBar
    'tooltip',     // teToolTip
    'trackbar',    // teTrackBar
    'traynotify',  // teTrayNotify
    'treeview',    // teTreeview
    'window'       // teWindow
  );

{ TFPGUIThemeServices }

function TFPGUIThemeServices.InitThemes: Boolean;
begin
  Result:=inherited InitThemes;
end;

function TFPGUIThemeServices.UseThemes: Boolean;
begin
  Result:=inherited UseThemes;
end;

function TFPGUIThemeServices.ThemedControlsEnabled: Boolean;
begin
  Result:=inherited ThemedControlsEnabled;
end;

destructor TFPGUIThemeServices.Destroy;
begin
  inherited Destroy;
end;

function TFPGUIThemeServices.GetDetailSize(Details: TThemedElementDetails
  ): TSize;
begin
  {$IFDEF FPGUIDEBUGTHEMES}
  writeln('GetDetailSize: ',ThemeDataNames[Details.Element],',',Details.Part,',',Details.State);
  {$ENDIF}
  Result:=inherited GetDetailSize(Details);
end;

procedure TFPGUIThemeServices.DrawElement(DC: HDC;
  Details: TThemedElementDetails; const R: TRect; ClipRect: PRect);
var
  ADC: TFPGUIDeviceContext;
  fpgRect: TfpgRect;
  fpgRect2: TfpgRect;
begin
  ADC:=TFPGUIDeviceContext(DC);
  fpgRect:=ADC.PrepareRectOffsets(R);
  {$IFDEF FPGUIDEBUGTHEMES}
  writeln('DrawElement:   ',ThemeDataNames[Details.Element],',',Details.Part,',',Details.State);
  {$ENDIF}
  case Details.Element of
    teRebar:
    begin
      case Details.Part of
        RP_GRIPPERVERT: begin
          ADC.fpgCanvas.DrawButtonFace(fpgRect,[]);
          InflateRect(fpgRect,-2,-2);
          fpgRect2:=fpgRect;
          fpgRect.Height:=fpgRect.Height div 2;
          fpgRect2.Top:=fpgRect.Top+fpgRect.Height+1;
          fpgRect2.Height:=fpgRect.Height;
          ADC.fpgCanvas.Color:=clText1;
          if (fpgRect.Width>2) and (fpgRect.Height>2) then begin
            ADC.fpgCanvas.DrawDirectionArrow(fpgRect,TArrowDirection.adUp);
          end;
          if (fpgRect2.Width>2) and (fpgRect2.Height>2) then begin
            ADC.fpgCanvas.DrawDirectionArrow(fpgRect2,TArrowDirection.adDown);
          end;
        end;
        RP_GRIPPER: begin
          ADC.fpgCanvas.DrawButtonFace(fpgRect,[]);
          InflateRect(fpgRect,-2,-2);
          fpgRect2:=fpgRect;
          fpgRect.Width:=fpgRect.Width div 2;
          fpgRect2.Left:=fpgRect.Left+fpgRect.Width+1;
          fpgRect2.Width:=fpgRect.Width;
          ADC.fpgCanvas.Color:=clText1;
          if (fpgRect.Width>2) and (fpgRect.Height>2) then begin
            ADC.fpgCanvas.DrawDirectionArrow(fpgRect,TArrowDirection.adLeft);
          end;
          if (fpgRect2.Width>2) and (fpgRect2.Height>2) then begin
            ADC.fpgCanvas.DrawDirectionArrow(fpgRect2,TArrowDirection.adRight);
          end;
        end;
        otherwise
          begin
            ADC.fpgCanvas.DrawButtonFace(fpgRect,[]);
          end;
      end;
    end;
    teButton:
    begin
      case Details.Part of
        BP_PUSHBUTTON: begin
          case Details.State of
            PBS_DEFAULTED: ADC.fpgCanvas.DrawButtonFace(fpgRect,[btfIsDefault]);
            PBS_DISABLED: ADC.fpgCanvas.DrawButtonFace(fpgRect,[btfDisabled]);
            PBS_HOT: ADC.fpgCanvas.DrawButtonFace(fpgRect,[btfHover]);
            PBS_NORMAL: ADC.fpgCanvas.DrawButtonFace(fpgRect,[]);
            PBS_PRESSED: ADC.fpgCanvas.DrawButtonFace(fpgRect,[btfIsPressed]);
          end;
        end;
        otherwise begin
          ADC.fpgCanvas.DrawButtonFace(fpgRect,[]);
        end;
      end;
    end;
    teToolBar:
    begin
      case Details.Part of
        TP_BUTTON: begin
          case Details.State of
            TS_DISABLED: begin
              ADC.fpgCanvas.Color:=clShadow2;
              ADC.fpgCanvas.FillRectangle(fpgRect);
            end;
            TS_NORMAL: begin
              ADC.fpgCanvas.Color:=clWindowBackground;
              ADC.fpgCanvas.FillRectangle(fpgRect);
            end;
            TS_PRESSED: ADC.fpgCanvas.DrawButtonFace(fpgRect,[btfIsPressed]);
          end;
        end;
        otherwise begin
          ADC.fpgCanvas.DrawButtonFace(fpgRect,[]);
        end;
      end;
    end;
    otherwise begin
      {$IFDEF FPGUIDEBUGTHEMES}
      writeln('Inherit DrawElement:   ',ThemeDataNames[Details.Element],',',Details.Part,',',Details.State);
      {$ENDIF}
      inherited DrawElement(DC, Details, R, ClipRect);
    end;
  end;
end;

function TFPGUIThemeServices.ContentRect(DC: HDC;
  Details: TThemedElementDetails; BoundingRect: TRect): TRect;
begin
  {$IFDEF FPGUIDEBUGTHEMES}
  writeln('Inherit ContentRect:   ',ThemeDataNames[Details.Element],',',Details.Part,',',Details.State);
  {$ENDIF}
  Result:=inherited ContentRect(DC, Details, BoundingRect);
end;

procedure TFPGUIThemeServices.InternalDrawParentBackground(Window: HWND;
  Target: HDC; Bounds: PRect);
begin
  inherited;
end;


end.
