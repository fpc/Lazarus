unit qt4;

{ Version : 1.18 }

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses Types;

{$MINENUMSIZE 4}


const
{$IFDEF MSWINDOWS}
  QtNamePrefix = '';
  QtShareName = 'libqt4intf.dll';
{$ENDIF}
{$IFDEF LINUX}
  QtNamePrefix = '';
  QtShareName = 'libqt4intf.so';
{$ENDIF}
{$IFDEF DARWIN}
  QtNamePrefix = '';
  QtShareName = '';
{$ENDIF}


type
  QHookH = TMethod;
  PSizePolicy = ^TSizePolicy;
  TSizePolicy = packed record
    Data: Word;
  end;
  
  PQColor = ^TQColor;
  TQColor = packed record
    ColorSpec : LongInt;
    Alpha : word;
    r,g,b : word; 
    Pad : word;
   end;
 
  QtHandle = integer;
  qreal = double;
  qrgb = longword;
  
  TCoreApplicationEventFilter = procedure(Msg:PChar;Res:PLongInt) cdecl;
    

  PIntArray = ^TIntArray;
  TIntArray = array of Integer;
  
  
  
const
  NullHook: QHookH = (Code: nil; Data: nil);

type

{$IFDEF DARWIN}
    EventHandlerRef                     = ^LongInt;
    EventRef                            = ^LongInt;
    RgnHandle                           = ^LongInt;
    MenuHandle                          = ^LongInt;
    MenuRef                             = MenuHandle;
    EventHandlerCallRef                 = ^LongInt;
{$ENDIF}


{$IFDEF MSWINDOWS}
  { Message structure }
  PMsg = ^tagMSG;
  tagMSG = packed record
    hwnd: LongWord;
    message: Cardinal;
    wParam: Longint;
    lParam: Longint;
    time: DWORD;
    pt: TPoint;
  end;

  WINHANDLE = type integer;
  HCURSOR = type WINHANDLE;
  HPALETTE = type WINHANDLE;
  HFONT = type WINHANDLE;
  HDC = type WINHANDLE;
  HBITMAP = type WINHANDLE;
  HBRUSH = type WINHANDLE;
  HPEN = type WINHANDLE;
  HRGN = type WINHANDLE;
{$ENDIF}



QBitArrayH = class(TObject) end;
QBrushH = class(TObject) end;
QByteArrayH = class(TObject) end;
QCharH = class(TObject) end;
QColorH = class(TObject) end;
QCursorH = class(TObject) end;
QDataStreamH = class(TObject) end;
QDateH = class(TObject) end;
QDateTimeH = class(TObject) end;
QDirH = class(TObject) end;
QEventH = class(TObject) end;
  QActionEventH = class(QEventH) end;
  QChildEventH = class(QEventH) end;
  QCloseEventH = class(QEventH) end;
  QDragLeaveEventH = class(QEventH) end;
  QDropEventH = class(QEventH) end;
    QDragMoveEventH = class(QDropEventH) end;
      QDragEnterEventH = class(QDragMoveEventH) end;
  QFileOpenEventH = class(QEventH) end;
  QFocusEventH = class(QEventH) end;
  QHelpEventH = class(QEventH) end;
  QHideEventH = class(QEventH) end;
  QHoverEventH = class(QEventH) end;
  QIconDragEventH = class(QEventH) end;
  QInputEventH = class(QEventH) end;
    QContextMenuEventH = class(QInputEventH) end;
    QKeyEventH = class(QInputEventH) end;
    QMouseEventH = class(QInputEventH) end;
    QTabletEventH = class(QInputEventH) end;
    QWheelEventH = class(QInputEventH) end;
  QInputMethodEventH = class(QEventH) end;
  QMoveEventH = class(QEventH) end;
  QPaintEventH = class(QEventH) end;
  QResizeEventH = class(QEventH) end;
  QShortcutEventH = class(QEventH) end;
  QShowEventH = class(QEventH) end;
  QStatusTipEventH = class(QEventH) end;
  QTimerEventH = class(QEventH) end;
  QWhatsThisClickedEventH = class(QEventH) end;
  QWindowStateChangeEventH = class(QEventH) end;
QFileIconProviderH = class(TObject) end;
QFontH = class(TObject) end;
QFontDatabaseH = class(TObject) end;
QFontInfoH = class(TObject) end;
QFontMetricsH = class(TObject) end;
QFontMetricsFH = class(TObject) end;
QGradientH = class(TObject) end;
  QConicalGradientH = class(QGradientH) end;
  QLinearGradientH = class(QGradientH) end;
  QRadialGradientH = class(QGradientH) end;
QIconH = class(TObject) end;
QIconEngineH = class(TObject) end;
QImageReaderH = class(TObject) end;
QImageWriterH = class(TObject) end;
QKeySequenceH = class(TObject) end;
QLatin1StringH = class(TObject) end;
QLayoutItemH = class(TObject) end;
  QLayoutH = class(QLayoutItemH) end;
    QBoxLayoutH = class(QLayoutH) end;
      QHBoxLayoutH = class(QBoxLayoutH) end;
      QVBoxLayoutH = class(QBoxLayoutH) end;
    QGridLayoutH = class(QLayoutH) end;
  QSpacerItemH = class(QLayoutItemH) end;
  QWidgetItemH = class(QLayoutItemH) end;
QLineH = class(TObject) end;
QLineFH = class(TObject) end;
QListH = class(TObject) end;
  QStringListH = class(QListH) end;
QListWidgetItemH = class(TObject) end;
QLocaleH = class(TObject) end;
QMatrixH = class(TObject) end;
QMimeSourceH = class(TObject) end;
QModelIndexH = class(TObject) end;
QObjectH = class(TObject) end;
  QAbstractItemDelegateH = class(QObjectH) end;
  QAbstractItemModelH = class(QObjectH) end;
  QAbstractTextDocumentLayoutH = class(QObjectH) end;
  QActionH = class(QObjectH) end;
  QActionGroupH = class(QObjectH) end;
  QButtonGroupH = class(QObjectH) end;
  QClipboardH = class(QObjectH) end;
  QCoreApplicationH = class(QObjectH) end;
    QApplicationH = class(QCoreApplicationH) end;
  QEventLoopH = class(QObjectH) end;
  QIODeviceH = class(QObjectH) end;
  QInputContextH = class(QObjectH) end;
  QItemSelectionModelH = class(QObjectH) end;
  QMimeDataH = class(QObjectH) end;
  QMovieH = class(QObjectH) end;
  QSessionManagerH = class(QObjectH) end;
  QStyleH = class(QObjectH) end;
  QTextDocumentH = class(QObjectH) end;
  QTextObjectH = class(QObjectH) end;
    QTextFrameH = class(QTextObjectH) end;
  QThreadH = class(QObjectH) end;
  QTimerH = class(QObjectH) end;
  QTranslatorH = class(QObjectH) end;
  QValidatorH = class(QObjectH) end;
    QDoubleValidatorH = class(QValidatorH) end;
    QIntValidatorH = class(QValidatorH) end;
    QRegExpValidatorH = class(QValidatorH) end;
  QWidgetH = class(QObjectH) end;
    QAbstractButtonH = class(QWidgetH) end;
      QCheckBoxH = class(QAbstractButtonH) end;
      QPushButtonH = class(QAbstractButtonH) end;
      QRadioButtonH = class(QAbstractButtonH) end;
      QToolButtonH = class(QAbstractButtonH) end;
    QAbstractSliderH = class(QWidgetH) end;
      QScrollBarH = class(QAbstractSliderH) end;
      QSliderH = class(QAbstractSliderH) end;
    QAbstractSpinBoxH = class(QWidgetH) end;
      QDoubleSpinBoxH = class(QAbstractSpinBoxH) end;
      QSpinBoxH = class(QAbstractSpinBoxH) end;
    QComboBoxH = class(QWidgetH) end;
    QDesktopWidgetH = class(QWidgetH) end;
    QDialogH = class(QWidgetH) end;
      QColorDialogH = class(QDialogH) end;
      QFileDialogH = class(QDialogH) end;
      QFontDialogH = class(QDialogH) end;
      QInputDialogH = class(QDialogH) end;
      QMessageBoxH = class(QDialogH) end;
      QProgressDialogH = class(QDialogH) end;
    QDockWidgetH = class(QWidgetH) end;
    QFrameH = class(QWidgetH) end;
      QAbstractScrollAreaH = class(QFrameH) end;
        QAbstractItemViewH = class(QAbstractScrollAreaH) end;
          QHeaderViewH = class(QAbstractItemViewH) end;
          QListViewH = class(QAbstractItemViewH) end;
            QListWidgetH = class(QListViewH) end;
          QTreeViewH = class(QAbstractItemViewH) end;
            QTreeWidgetH = class(QTreeViewH) end;
        QTextEditH = class(QAbstractScrollAreaH) end;
          QTextBrowserH = class(QTextEditH) end;
      QLCDNumberH = class(QFrameH) end;
      QLabelH = class(QFrameH) end;
      QSplitterH = class(QFrameH) end;
      QToolBoxH = class(QFrameH) end;
    QGroupBoxH = class(QWidgetH) end;
    QLineEditH = class(QWidgetH) end;
    QMainWindowH = class(QWidgetH) end;
    QMenuH = class(QWidgetH) end;
    QMenuBarH = class(QWidgetH) end;
    QProgressBarH = class(QWidgetH) end;
    QSizeGripH = class(QWidgetH) end;
    QSplitterHandleH = class(QWidgetH) end;
    QStatusBarH = class(QWidgetH) end;
    QTabBarH = class(QWidgetH) end;
    QTabWidgetH = class(QWidgetH) end;
    QToolBarH = class(QWidgetH) end;
    QWorkspaceH = class(QWidgetH) end;
QPaintDeviceH = class(TObject) end;
  QImageH = class(QPaintDeviceH) end;
  QPictureH = class(QPaintDeviceH) end;
  QPixmapH = class(QPaintDeviceH) end;
    QBitmapH = class(QPixmapH) end;
  QPrinterH = class(QPaintDeviceH) end;
QPaintEngineH = class(TObject) end;
QPainterH = class(TObject) end;
QPainterPathH = class(TObject) end;
QPaletteH = class(TObject) end;
QPenH = class(TObject) end;
QPictureIOH = class(TObject) end;
QPointH = class(TObject) end;
QPointFH = class(TObject) end;
QPrintEngineH = class(TObject) end;
QRectH = class(TObject) end;
QRectFH = class(TObject) end;
QRegExpH = class(TObject) end;
QRegionH = class(TObject) end;
QSizeH = class(TObject) end;
QSizeFH = class(TObject) end;
QSizePolicyH = class(TObject) end;
QStringH = class(TObject) end;
QTextBlockH = class(TObject) end;
QTextCursorH = class(TObject) end;
QTextFormatH = class(TObject) end;
  QTextCharFormatH = class(QTextFormatH) end;
QTextOptionH = class(TObject) end;
QTimeH = class(TObject) end;
QToolTipH = class(TObject) end;
QTreeWidgetItemH = class(TObject) end;
QUrlH = class(TObject) end;
QVariantH = class(TObject) end;
QVectorH = class(TObject) end;
  QPolygonH = class(QVectorH) end;
  QPolygonFH = class(QVectorH) end;


QObject_hookH = class(TObject) end;
QEvent_hookH = class(QObject_hookH) end;
QTimerEvent_hookH = class(QEvent_hookH) end;
QChildEvent_hookH = class(QEvent_hookH) end;
QEventLoop_hookH = class(QObject_hookH) end;
QCoreApplication_hookH = class(QObject_hookH) end;
QTimer_hookH = class(QObject_hookH) end;
QApplication_hookH = class(QCoreApplication_hookH) end;
QWidget_hookH = class(QObject_hookH) end;
QAction_hookH = class(QObject_hookH) end;
QClipboard_hookH = class(QObject_hookH) end;
QDesktopWidget_hookH = class(QWidget_hookH) end;
QFrame_hookH = class(QWidget_hookH) end;
QAbstractScrollArea_hookH = class(QFrame_hookH) end;
QAbstractSlider_hookH = class(QWidget_hookH) end;
QScrollBar_hookH = class(QAbstractSlider_hookH) end;
QMenu_hookH = class(QWidget_hookH) end;
QMenuBar_hookH = class(QWidget_hookH) end;
QButtonGroup_hookH = class(QObject_hookH) end;
QAbstractButton_hookH = class(QWidget_hookH) end;
QPushButton_hookH = class(QAbstractButton_hookH) end;
QLineEdit_hookH = class(QWidget_hookH) end;
QTextEdit_hookH = class(QAbstractScrollArea_hookH) end;
QMainWindow_hookH = class(QWidget_hookH) end;
QToolBar_hookH = class(QWidget_hookH) end;
QLCDNumber_hookH = class(QFrame_hookH) end;
QAbstractSpinBox_hookH = class(QWidget_hookH) end;
QSpinBox_hookH = class(QAbstractSpinBox_hookH) end;
QDoubleSpinBox_hookH = class(QAbstractSpinBox_hookH) end;
QSplitter_hookH = class(QFrame_hookH) end;
QSplitterHandle_hookH = class(QWidget_hookH) end;
QWorkspace_hookH = class(QWidget_hookH) end;
QComboBox_hookH = class(QWidget_hookH) end;
QCheckBox_hookH = class(QAbstractButton_hookH) end;
QSlider_hookH = class(QAbstractSlider_hookH) end;
QTextBrowser_hookH = class(QTextEdit_hookH) end;
QLabel_hookH = class(QFrame_hookH) end;
QGroupBox_hookH = class(QWidget_hookH) end;
QTabWidget_hookH = class(QWidget_hookH) end;
QTabBar_hookH = class(QWidget_hookH) end;
QProgressBar_hookH = class(QWidget_hookH) end;
QStatusBar_hookH = class(QWidget_hookH) end;
QToolBox_hookH = class(QFrame_hookH) end;
QToolButton_hookH = class(QAbstractButton_hookH) end;
QAbstractItemView_hookH = class(QAbstractScrollArea_hookH) end;
QListView_hookH = class(QAbstractItemView_hookH) end;
QListWidgetItem_hookH = class(QObject_hookH) end;
QListWidget_hookH = class(QListView_hookH) end;
QTreeView_hookH = class(QAbstractItemView_hookH) end;
QTreeWidgetItem_hookH = class(QObject_hookH) end;
QTreeWidget_hookH = class(QTreeView_hookH) end;
QDialog_hookH = class(QWidget_hookH) end;
QProgressDialog_hookH = class(QDialog_hookH) end;
QIODevice_hookH = class(QObject_hookH) end;

  TPictureIOHandler = procedure(Pic: QPictureIOH) cdecl;
  TEventFilterMethod = function (Sender: QObjectH; Event: QEventH): Boolean of object cdecl;

function QObject_hook_create(handle : QObjectH) : QObject_hookH; cdecl; external QtShareName name QtNamePrefix + 'QObject_hook_create';
procedure QObject_hook_destroy(handle : QObject_hookH ); cdecl; external QtShareName name QtNamePrefix + 'QObject_hook_destroy';
procedure QObject_hook_hook_events(handle : QObject_hookH; hook : QHookH); cdecl; external QtShareName name QtNamePrefix + 'QObject_hook_hook_events';
procedure QObject_hook_hook_destroyed(handle : QObject_hookH; hook : QHookH); cdecl; external QtShareName name QtNamePrefix + 'QObject_hook_hook_destroyed';

type
  QtGlobalColor = ( // Qt::GlobalColor (1)
    Qtcolor0, Qtcolor1, Qtblack, Qtwhite, QtdarkGray, Qtgray, QtlightGray, Qtred, Qtgreen, Qtblue, Qtcyan, Qtmagenta, Qtyellow, QtdarkRed, QtdarkGreen, QtdarkBlue, QtdarkCyan, QtdarkMagenta, 
    QtdarkYellow, Qttransparent );

  QtSortOrder = ( // Qt::SortOrder (1)
    QtAscendingOrder, QtDescendingOrder );

  QtTextElideMode = ( // Qt::TextElideMode (1)
    QtElideLeft, QtElideRight, QtElideMiddle );

  QtBGMode = ( // Qt::BGMode (1)
    QtTransparentMode, QtOpaqueMode );

  QtArrowType = ( // Qt::ArrowType (1)
    QtNoArrow, QtUpArrow, QtDownArrow, QtLeftArrow, QtRightArrow );

  QtUIEffect = ( // Qt::UIEffect (1)
    QtUI_General, QtUI_AnimateMenu, QtUI_FadeMenu, QtUI_AnimateCombo, QtUI_AnimateTooltip, QtUI_FadeTooltip, QtUI_AnimateToolBox );

  QtTextFormat = ( // Qt::TextFormat (1)
    QtPlainText, QtRichText, QtAutoText, QtLogText );

  QtAspectRatioMode = ( // Qt::AspectRatioMode (1)
    QtIgnoreAspectRatio, QtKeepAspectRatio, QtKeepAspectRatioByExpanding );

  QtAnchorAttribute = ( // Qt::AnchorAttribute (1)
    QtAnchorName, QtAnchorHref );

  QtDateFormat = ( // Qt::DateFormat (1)
    QtTextDate, QtISODate, QtLocalDate );

  QtTimeSpec = ( // Qt::TimeSpec (1)
    QtLocalTime, QtUTC );

  QtScrollBarPolicy = ( // Qt::ScrollBarPolicy (1)
    QtScrollBarAsNeeded, QtScrollBarAlwaysOff, QtScrollBarAlwaysOn );

  QtCaseSensitivity = ( // Qt::CaseSensitivity (1)
    QtCaseInsensitive, QtCaseSensitive );

  QtConnectionType = ( // Qt::ConnectionType (1)
    QtAutoConnection, QtDirectConnection, QtQueuedConnection, QtAutoCompatConnection );

  QtShortcutContext = ( // Qt::ShortcutContext (1)
    QtWidgetShortcut, QtWindowShortcut, QtApplicationShortcut );

  QtFillRule = ( // Qt::FillRule (1)
    QtOddEvenFill, QtWindingFill );

  QtClipOperation = ( // Qt::ClipOperation (1)
    QtNoClip, QtReplaceClip, QtIntersectClip, QtUniteClip );

  QtTransformationMode = ( // Qt::TransformationMode (1)
    QtFastTransformation, QtSmoothTransformation );

  QtFocusReason = ( // Qt::FocusReason (1)
    QtMouseFocusReason, QtTabFocusReason, QtBacktabFocusReason, QtActiveWindowFocusReason, QtPopupFocusReason, QtShortcutFocusReason, QtMenuBarFocusReason, QtOtherFocusReason, QtNoFocusReason );

  QtContextMenuPolicy = ( // Qt::ContextMenuPolicy (1)
    QtNoContextMenu, QtDefaultContextMenu, QtActionsContextMenu, QtCustomContextMenu );

  QtInputMethodQuery = ( // Qt::InputMethodQuery (1)
    QtImMicroFocus, QtImFont, QtImCursorPosition, QtImSurroundingText, QtImCurrentSelection );

  QtToolButtonStyle = ( // Qt::ToolButtonStyle (1)
    QtToolButtonIconOnly, QtToolButtonTextOnly, QtToolButtonTextBesideIcon, QtToolButtonTextUnderIcon );

  QtLayoutDirection = ( // Qt::LayoutDirection (1)
    QtLeftToRight, QtRightToLeft );

  QtCheckState = ( // Qt::CheckState (1)
    QtUnchecked, QtPartiallyChecked, QtChecked );

  QtWindowModality = ( // Qt::WindowModality (1)
    QtNonModal, QtWindowModal, QtApplicationModal );

type
  QtKeyboardModifier = cardinal; // Qt::KeyboardModifier
  QtKeyboardModifiers = QtKeyboardModifier; //QFlags<> (3)
const
  QtNoModifier =   $00000000;
  QtShiftModifier =   $02000000;
  QtControlModifier =   $04000000;
  QtAltModifier =   $08000000;
  QtMetaModifier =   $10000000;
  QtKeypadModifier =   $20000000;
  QtKeyboardModifierMask =   $fe000000;

type
  QtMouseButton = cardinal; // Qt::MouseButton
  QtMouseButtons = QtMouseButton; //QFlags<> (3)
const
  QtNoButton =   $00000000;
  QtLeftButton =   $00000001;
  QtRightButton =   $00000002;
  QtMidButton =   $00000004;
  QtXButton1 =   $00000008;
  QtXButton2 =   $00000010;
  QtMouseButtonMask =   $000000ff;

type
  QtOrientation = cardinal; // Qt::Orientation
  QtOrientations = QtOrientation; //QFlags<> (3)
const
  QtHorizontal =   $1;
  QtVertical =   $2;

type
  QtWindowState = cardinal; // Qt::WindowState
  QtWindowStates = QtWindowState; //QFlags<> (3)
const
  QtWindowNoState =   $00000000;
  QtWindowMinimized =   $00000001;
  QtWindowMaximized =   $00000002;
  QtWindowFullScreen =   $00000004;
  QtWindowActive =   $00000008;

type
  QtImageConversionFlag = cardinal; // Qt::ImageConversionFlag
  QtImageConversionFlags = QtImageConversionFlag; //QFlags<> (3)
const
  QtColorMode_Mask =   $00000003;
  QtAutoColor =   $00000000;
  QtColorOnly =   $00000003;
  QtMonoOnly =   $00000002;
  QtAlphaDither_Mask =   $0000000c;
  QtThresholdAlphaDither =   $00000000;
  QtOrderedAlphaDither =   $00000004;
  QtDiffuseAlphaDither =   $00000008;
  QtNoAlpha =   $0000000c;
  QtDither_Mask =   $00000030;
  QtDiffuseDither =   $00000000;
  QtOrderedDither =   $00000010;
  QtThresholdDither =   $00000020;
  QtDitherMode_Mask =   $000000c0;
  QtAutoDither =   $00000000;
  QtPreferDither =   $00000040;
  QtAvoidDither =   $00000080;


type
  QtPenStyle = (  //Qt::PenStyle (2)
    QtNoPen,
    QtSolidLine,
    QtDashLine,
    QtDotLine,
    QtDashDotLine,
    QtDashDotDotLine,
    QtCustomDashLine,
    QtMPenStyle = $0f );

  QtPenCapStyle = (  //Qt::PenCapStyle (2s)
    QtFlatCap = $00,
    QtSquareCap = $10,
    QtRoundCap = $20,
    QtMPenCapStyle = $30 );

  QtPenJoinStyle = (  //Qt::PenJoinStyle (2s)
    QtMiterJoin = $00,
    QtBevelJoin = $40,
    QtRoundJoin = $80,
    QtMPenJoinStyle = $c0 );

  QtBrushStyle = (  //Qt::BrushStyle (2)
    QtNoBrush,
    QtSolidPattern,
    QtDense1Pattern,
    QtDense2Pattern,
    QtDense3Pattern,
    QtDense4Pattern,
    QtDense5Pattern,
    QtDense6Pattern,
    QtDense7Pattern,
    QtHorPattern,
    QtVerPattern,
    QtCrossPattern,
    QtBDiagPattern,
    QtFDiagPattern,
    QtDiagCrossPattern,
    QtLinearGradientPattern,
    QtRadialGradientPattern,
    QtConicalGradientPattern,
    QtTexturePattern = 24 );

  QtDayOfWeek = (  //Qt::DayOfWeek (2s)
    QtMonday = 1,
    QtTuesday = 2,
    QtWednesday = 3,
    QtThursday = 4,
    QtFriday = 5,
    QtSaturday = 6,
    QtSunday = 7 );

  QtCorner = (  //Qt::Corner (2s)
    QtTopLeftCorner = $00000,
    QtTopRightCorner = $00001,
    QtBottomLeftCorner = $00002,
    QtBottomRightCorner = $00003 );

type
  QtDropAction = cardinal; // Qt::DropAction
  QtDropActions = QtDropAction; //QFlags<> (3)
const
  QtCopyAction =   $1;
  QtMoveAction =   $2;
  QtLinkAction =   $4;
  QtActionMask =   $ff;
  QtTargetMoveAction =   $8002;
  QtIgnoreAction =   $0;


type
  QtItemDataRole = (  //Qt::ItemDataRole (2s)
    QtDisplayRole = 0,
    QtDecorationRole = 1,
    QtEditRole = 2,
    QtToolTipRole = 3,
    QtStatusTipRole = 4,
    QtWhatsThisRole = 5,
    QtFontRole = 6,
    QtTextAlignmentRole = 7,
    QtBackgroundColorRole = 8,
    QtTextColorRole = 9,
    QtCheckStateRole = 10,
    QtAccessibleTextRole = 11,
    QtAccessibleDescriptionRole = 12,
    QtSizeHintRole = 13,
    QtUserRole = 32 );

type
  QtItemFlag = cardinal; // Qt::ItemFlag
  QtItemFlags = QtItemFlag; //QFlags<> (3)
const
  QtItemIsSelectable =   1;
  QtItemIsEditable =   2;
  QtItemIsDragEnabled =   4;
  QtItemIsDropEnabled =   8;
  QtItemIsUserCheckable =   16;
  QtItemIsEnabled =   32;
  QtItemIsTristate =   64;

type
  QtMatchFlag = cardinal; // Qt::MatchFlag
  QtMatchFlags = QtMatchFlag; //QFlags<> (3)
const
  QtMatchExactly =   0;
  QtMatchContains =   1;
  QtMatchStartsWith =   2;
  QtMatchEndsWith =   3;
  QtMatchRegExp =   4;
  QtMatchWildcard =   5;
  QtMatchCaseSensitive =   16;
  QtMatchWrap =   32;
  QtMatchRecursive =   64;


type
  QInternalRelayoutType = ( // QInternal::RelayoutType (1)
    QInternalRelayoutNormal, QInternalRelayoutDragging, QInternalRelayoutDropped );

  QInternalPaintDeviceFlags = (  //QInternal::PaintDeviceFlags (2s)
    QInternalUnknownDevice = $00,
    QInternalWidget = $01,
    QInternalPixmap = $02,
    QInternalImage = $03,
    QInternalPrinter = $04,
    QInternalPicture = $05,
    QInternalPbuffer = $06 );

type
  QtModifier = cardinal; //  Qt::Modifier (4)

const
    QtMETA = 268435456 { $10000000 };
    QtSHIFT = 33554432 { $2000000 };
    QtCTRL = 67108864 { $4000000 };
    QtALT = 134217728 { $8000000 };
    QtMODIFIER_MASK = 4261412864 { $fe000000 };
    QtUNICODE_ACCEL = 0 { $0 };

type
  QtFocusPolicy = cardinal; //  Qt::FocusPolicy (4)

const
    QtNoFocus = 0 { $0 };
    QtTabFocus = 1 { $1 };
    QtClickFocus = 2 { $2 };
    QtStrongFocus = 11 { $b };
    QtWheelFocus = 15 { $f };

type
  QtAlignmentFlag = cardinal; //  Qt::AlignmentFlag (4)
  QtAlignment = QtAlignmentFlag; // QFlags<>

const
    QtAlignLeft = 1 { $1 };
    QtAlignLeading = 1 { $1 };
    QtAlignRight = 2 { $2 };
    QtAlignTrailing = 2 { $2 };
    QtAlignHCenter = 4 { $4 };
    QtAlignJustify = 8 { $8 };
    QtAlignAbsolute = 16 { $10 };
    QtAlignHorizontal_Mask = 31 { $1f };
    QtAlignTop = 32 { $20 };
    QtAlignBottom = 64 { $40 };
    QtAlignVCenter = 128 { $80 };
    QtAlignVertical_Mask = 224 { $e0 };
    QtAlignCenter = 132 { $84 };

type
  QtWindowType = cardinal; //  Qt::WindowType (4)
  QtWindowFlags = QtWindowType; // QFlags<>

const
    QtWidget = 0 { $0 };
    QtWindow = 1 { $1 };
    QtDialog = 3 { $3 };
    QtSheet = 5 { $5 };
    QtDrawer = 7 { $7 };
    QtPopup = 9 { $9 };
    QtTool = 11 { $b };
    QtToolTip = 13 { $d };
    QtSplashScreen = 15 { $f };
    QtDesktop = 17 { $11 };
    QtSubWindow = 18 { $12 };
    QtWindowType_Mask = 255 { $ff };
    QtMSWindowsFixedSizeDialogHint = 256 { $100 };
    QtMSWindowsOwnDC = 512 { $200 };
    QtX11BypassWindowManagerHint = 1024 { $400 };
    QtFramelessWindowHint = 2048 { $800 };
    QtWindowTitleHint = 4096 { $1000 };
    QtWindowSystemMenuHint = 8192 { $2000 };
    QtWindowMinimizeButtonHint = 16384 { $4000 };
    QtWindowMaximizeButtonHint = 32768 { $8000 };
    QtWindowMinMaxButtonsHint = 49152 { $c000 };
    QtWindowContextHelpButtonHint = 65536 { $10000 };
    QtWindowShadeButtonHint = 131072 { $20000 };
    QtWindowStaysOnTopHint = 262144 { $40000 };

type
  QtWidgetAttribute = cardinal; //  Qt::WidgetAttribute (4)

const
    QtWA_Disabled = 0 { $0 };
    QtWA_UnderMouse = 1 { $1 };
    QtWA_MouseTracking = 2 { $2 };
    QtWA_ContentsPropagated = 3 { $3 };
    QtWA_OpaquePaintEvent = 4 { $4 };
    QtWA_NoBackground = 4 { $4 };
    QtWA_StaticContents = 5 { $5 };
    QtWA_LaidOut = 7 { $7 };
    QtWA_PaintOnScreen = 8 { $8 };
    QtWA_NoSystemBackground = 9 { $9 };
    QtWA_UpdatesDisabled = 10 { $a };
    QtWA_Mapped = 11 { $b };
    QtWA_MacNoClickThrough = 12 { $c };
    QtWA_PaintOutsidePaintEvent = 13 { $d };
    QtWA_InputMethodEnabled = 14 { $e };
    QtWA_WState_Visible = 15 { $f };
    QtWA_WState_Hidden = 16 { $10 };
    QtWA_ForceDisabled = 32 { $20 };
    QtWA_KeyCompression = 33 { $21 };
    QtWA_PendingMoveEvent = 34 { $22 };
    QtWA_PendingResizeEvent = 35 { $23 };
    QtWA_SetPalette = 36 { $24 };
    QtWA_SetFont = 37 { $25 };
    QtWA_SetCursor = 38 { $26 };
    QtWA_NoChildEventsFromChildren = 39 { $27 };
    QtWA_WindowModified = 41 { $29 };
    QtWA_Resized = 42 { $2a };
    QtWA_Moved = 43 { $2b };
    QtWA_PendingUpdate = 44 { $2c };
    QtWA_InvalidSize = 45 { $2d };
    QtWA_MacMetalStyle = 46 { $2e };
    QtWA_CustomWhatsThis = 47 { $2f };
    QtWA_LayoutOnEntireRect = 48 { $30 };
    QtWA_OutsideWSRange = 49 { $31 };
    QtWA_GrabbedShortcut = 50 { $32 };
    QtWA_TransparentForMouseEvents = 51 { $33 };
    QtWA_PaintUnclipped = 52 { $34 };
    QtWA_SetWindowIcon = 53 { $35 };
    QtWA_NoMouseReplay = 54 { $36 };
    QtWA_DeleteOnClose = 55 { $37 };
    QtWA_RightToLeft = 56 { $38 };
    QtWA_SetLayoutDirection = 57 { $39 };
    QtWA_NoChildEventsForParent = 58 { $3a };
    QtWA_ForceUpdatesDisabled = 59 { $3b };
    QtWA_WState_Created = 60 { $3c };
    QtWA_WState_CompressKeys = 61 { $3d };
    QtWA_WState_InPaintEvent = 62 { $3e };
    QtWA_WState_Reparented = 63 { $3f };
    QtWA_WState_ConfigPending = 64 { $40 };
    QtWA_WState_Polished = 66 { $42 };
    QtWA_WState_DND = 67 { $43 };
    QtWA_WState_OwnSizePolicy = 68 { $44 };
    QtWA_WState_ExplicitShowHide = 69 { $45 };
    QtWA_ShowModal = 70 { $46 };
    QtWA_MouseNoMask = 71 { $47 };
    QtWA_GroupLeader = 72 { $48 };
    QtWA_NoMousePropagation = 73 { $49 };
    QtWA_Hover = 74 { $4a };
    QtWA_InputMethodTransparent = 75 { $4b };
    QtWA_QuitOnClose = 76 { $4c };
    QtWA_KeyboardFocusChange = 77 { $4d };
    QtWA_AcceptDrops = 78 { $4e };
    QtWA_DropSiteRegistered = 79 { $4f };
    QtWA_ForceAcceptDrops = 79 { $4f };
    QtWA_WindowPropagation = 80 { $50 };
    QtWA_NoX11EventCompression = 81 { $51 };
    QtWA_TintedBackground = 82 { $52 };
    QtWA_X11OpenGLOverlay = 83 { $53 };
    QtWA_AttributeCount = 84 { $54 };

type
  QtKey = cardinal; //  Qt::Key (4)

const
    QtKey_Escape = 16777216 { $1000000 };
    QtKey_Tab = 16777217 { $1000001 };
    QtKey_Backtab = 16777218 { $1000002 };
    QtKey_Backspace = 16777219 { $1000003 };
    QtKey_Return = 16777220 { $1000004 };
    QtKey_Enter = 16777221 { $1000005 };
    QtKey_Insert = 16777222 { $1000006 };
    QtKey_Delete = 16777223 { $1000007 };
    QtKey_Pause = 16777224 { $1000008 };
    QtKey_Print = 16777225 { $1000009 };
    QtKey_SysReq = 16777226 { $100000a };
    QtKey_Clear = 16777227 { $100000b };
    QtKey_Home = 16777232 { $1000010 };
    QtKey_End = 16777233 { $1000011 };
    QtKey_Left = 16777234 { $1000012 };
    QtKey_Up = 16777235 { $1000013 };
    QtKey_Right = 16777236 { $1000014 };
    QtKey_Down = 16777237 { $1000015 };
    QtKey_PageUp = 16777238 { $1000016 };
    QtKey_PageDown = 16777239 { $1000017 };
    QtKey_Shift = 16777248 { $1000020 };
    QtKey_Control = 16777249 { $1000021 };
    QtKey_Meta = 16777250 { $1000022 };
    QtKey_Alt = 16777251 { $1000023 };
    QtKey_CapsLock = 16777252 { $1000024 };
    QtKey_NumLock = 16777253 { $1000025 };
    QtKey_ScrollLock = 16777254 { $1000026 };
    QtKey_F1 = 16777264 { $1000030 };
    QtKey_F2 = 16777265 { $1000031 };
    QtKey_F3 = 16777266 { $1000032 };
    QtKey_F4 = 16777267 { $1000033 };
    QtKey_F5 = 16777268 { $1000034 };
    QtKey_F6 = 16777269 { $1000035 };
    QtKey_F7 = 16777270 { $1000036 };
    QtKey_F8 = 16777271 { $1000037 };
    QtKey_F9 = 16777272 { $1000038 };
    QtKey_F10 = 16777273 { $1000039 };
    QtKey_F11 = 16777274 { $100003a };
    QtKey_F12 = 16777275 { $100003b };
    QtKey_F13 = 16777276 { $100003c };
    QtKey_F14 = 16777277 { $100003d };
    QtKey_F15 = 16777278 { $100003e };
    QtKey_F16 = 16777279 { $100003f };
    QtKey_F17 = 16777280 { $1000040 };
    QtKey_F18 = 16777281 { $1000041 };
    QtKey_F19 = 16777282 { $1000042 };
    QtKey_F20 = 16777283 { $1000043 };
    QtKey_F21 = 16777284 { $1000044 };
    QtKey_F22 = 16777285 { $1000045 };
    QtKey_F23 = 16777286 { $1000046 };
    QtKey_F24 = 16777287 { $1000047 };
    QtKey_F25 = 16777288 { $1000048 };
    QtKey_F26 = 16777289 { $1000049 };
    QtKey_F27 = 16777290 { $100004a };
    QtKey_F28 = 16777291 { $100004b };
    QtKey_F29 = 16777292 { $100004c };
    QtKey_F30 = 16777293 { $100004d };
    QtKey_F31 = 16777294 { $100004e };
    QtKey_F32 = 16777295 { $100004f };
    QtKey_F33 = 16777296 { $1000050 };
    QtKey_F34 = 16777297 { $1000051 };
    QtKey_F35 = 16777298 { $1000052 };
    QtKey_Super_L = 16777299 { $1000053 };
    QtKey_Super_R = 16777300 { $1000054 };
    QtKey_Menu = 16777301 { $1000055 };
    QtKey_Hyper_L = 16777302 { $1000056 };
    QtKey_Hyper_R = 16777303 { $1000057 };
    QtKey_Help = 16777304 { $1000058 };
    QtKey_Direction_L = 16777305 { $1000059 };
    QtKey_Direction_R = 16777312 { $1000060 };
    QtKey_Space = 32 { $20 };
    QtKey_Any = 32 { $20 };
    QtKey_Exclam = 33 { $21 };
    QtKey_QuoteDbl = 34 { $22 };
    QtKey_NumberSign = 35 { $23 };
    QtKey_Dollar = 36 { $24 };
    QtKey_Percent = 37 { $25 };
    QtKey_Ampersand = 38 { $26 };
    QtKey_Apostrophe = 39 { $27 };
    QtKey_ParenLeft = 40 { $28 };
    QtKey_ParenRight = 41 { $29 };
    QtKey_Asterisk = 42 { $2a };
    QtKey_Plus = 43 { $2b };
    QtKey_Comma = 44 { $2c };
    QtKey_Minus = 45 { $2d };
    QtKey_Period = 46 { $2e };
    QtKey_Slash = 47 { $2f };
    QtKey_0 = 48 { $30 };
    QtKey_1 = 49 { $31 };
    QtKey_2 = 50 { $32 };
    QtKey_3 = 51 { $33 };
    QtKey_4 = 52 { $34 };
    QtKey_5 = 53 { $35 };
    QtKey_6 = 54 { $36 };
    QtKey_7 = 55 { $37 };
    QtKey_8 = 56 { $38 };
    QtKey_9 = 57 { $39 };
    QtKey_Colon = 58 { $3a };
    QtKey_Semicolon = 59 { $3b };
    QtKey_Less = 60 { $3c };
    QtKey_Equal = 61 { $3d };
    QtKey_Greater = 62 { $3e };
    QtKey_Question = 63 { $3f };
    QtKey_At = 64 { $40 };
    QtKey_A = 65 { $41 };
    QtKey_B = 66 { $42 };
    QtKey_C = 67 { $43 };
    QtKey_D = 68 { $44 };
    QtKey_E = 69 { $45 };
    QtKey_F = 70 { $46 };
    QtKey_G = 71 { $47 };
    QtKey_H = 72 { $48 };
    QtKey_I = 73 { $49 };
    QtKey_J = 74 { $4a };
    QtKey_K = 75 { $4b };
    QtKey_L = 76 { $4c };
    QtKey_M = 77 { $4d };
    QtKey_N = 78 { $4e };
    QtKey_O = 79 { $4f };
    QtKey_P = 80 { $50 };
    QtKey_Q = 81 { $51 };
    QtKey_R = 82 { $52 };
    QtKey_S = 83 { $53 };
    QtKey_T = 84 { $54 };
    QtKey_U = 85 { $55 };
    QtKey_V = 86 { $56 };
    QtKey_W = 87 { $57 };
    QtKey_X = 88 { $58 };
    QtKey_Y = 89 { $59 };
    QtKey_Z = 90 { $5a };
    QtKey_BracketLeft = 91 { $5b };
    QtKey_Backslash = 92 { $5c };
    QtKey_BracketRight = 93 { $5d };
    QtKey_AsciiCircum = 94 { $5e };
    QtKey_Underscore = 95 { $5f };
    QtKey_QuoteLeft = 96 { $60 };
    QtKey_BraceLeft = 123 { $7b };
    QtKey_Bar = 124 { $7c };
    QtKey_BraceRight = 125 { $7d };
    QtKey_AsciiTilde = 126 { $7e };
    QtKey_nobreakspace = 160 { $a0 };
    QtKey_exclamdown = 161 { $a1 };
    QtKey_cent = 162 { $a2 };
    QtKey_sterling = 163 { $a3 };
    QtKey_currency = 164 { $a4 };
    QtKey_yen = 165 { $a5 };
    QtKey_brokenbar = 166 { $a6 };
    QtKey_section = 167 { $a7 };
    QtKey_diaeresis = 168 { $a8 };
    QtKey_copyright = 169 { $a9 };
    QtKey_ordfeminine = 170 { $aa };
    QtKey_guillemotleft = 171 { $ab };
    QtKey_notsign = 172 { $ac };
    QtKey_hyphen = 173 { $ad };
    QtKey_registered = 174 { $ae };
    QtKey_macron = 175 { $af };
    QtKey_degree = 176 { $b0 };
    QtKey_plusminus = 177 { $b1 };
    QtKey_twosuperior = 178 { $b2 };
    QtKey_threesuperior = 179 { $b3 };
    QtKey_acute = 180 { $b4 };
    QtKey_mu = 181 { $b5 };
    QtKey_paragraph = 182 { $b6 };
    QtKey_periodcentered = 183 { $b7 };
    QtKey_cedilla = 184 { $b8 };
    QtKey_onesuperior = 185 { $b9 };
    QtKey_masculine = 186 { $ba };
    QtKey_guillemotright = 187 { $bb };
    QtKey_onequarter = 188 { $bc };
    QtKey_onehalf = 189 { $bd };
    QtKey_threequarters = 190 { $be };
    QtKey_questiondown = 191 { $bf };
    QtKey_Agrave = 192 { $c0 };
    QtKey_Aacute = 193 { $c1 };
    QtKey_Acircumflex = 194 { $c2 };
    QtKey_Atilde = 195 { $c3 };
    QtKey_Adiaeresis = 196 { $c4 };
    QtKey_Aring = 197 { $c5 };
    QtKey_AE = 198 { $c6 };
    QtKey_Ccedilla = 199 { $c7 };
    QtKey_Egrave = 200 { $c8 };
    QtKey_Eacute = 201 { $c9 };
    QtKey_Ecircumflex = 202 { $ca };
    QtKey_Ediaeresis = 203 { $cb };
    QtKey_Igrave = 204 { $cc };
    QtKey_Iacute = 205 { $cd };
    QtKey_Icircumflex = 206 { $ce };
    QtKey_Idiaeresis = 207 { $cf };
    QtKey_ETH = 208 { $d0 };
    QtKey_Ntilde = 209 { $d1 };
    QtKey_Ograve = 210 { $d2 };
    QtKey_Oacute = 211 { $d3 };
    QtKey_Ocircumflex = 212 { $d4 };
    QtKey_Otilde = 213 { $d5 };
    QtKey_Odiaeresis = 214 { $d6 };
    QtKey_multiply = 215 { $d7 };
    QtKey_Ooblique = 216 { $d8 };
    QtKey_Ugrave = 217 { $d9 };
    QtKey_Uacute = 218 { $da };
    QtKey_Ucircumflex = 219 { $db };
    QtKey_Udiaeresis = 220 { $dc };
    QtKey_Yacute = 221 { $dd };
    QtKey_THORN = 222 { $de };
    QtKey_ssharp = 223 { $df };
    QtKey_division = 247 { $f7 };
    QtKey_ydiaeresis = 255 { $ff };
    QtKey_AltGr = 16781571 { $1001103 };
    QtKey_Multi_key = 16781600 { $1001120 };
    QtKey_Codeinput = 16781623 { $1001137 };
    QtKey_SingleCandidate = 16781628 { $100113c };
    QtKey_MultipleCandidate = 16781629 { $100113d };
    QtKey_PreviousCandidate = 16781630 { $100113e };
    QtKey_Mode_switch = 16781694 { $100117e };
    QtKey_Kanji = 16781601 { $1001121 };
    QtKey_Muhenkan = 16781602 { $1001122 };
    QtKey_Henkan = 16781603 { $1001123 };
    QtKey_Romaji = 16781604 { $1001124 };
    QtKey_Hiragana = 16781605 { $1001125 };
    QtKey_Katakana = 16781606 { $1001126 };
    QtKey_Hiragana_Katakana = 16781607 { $1001127 };
    QtKey_Zenkaku = 16781608 { $1001128 };
    QtKey_Hankaku = 16781609 { $1001129 };
    QtKey_Zenkaku_Hankaku = 16781610 { $100112a };
    QtKey_Touroku = 16781611 { $100112b };
    QtKey_Massyo = 16781612 { $100112c };
    QtKey_Kana_Lock = 16781613 { $100112d };
    QtKey_Kana_Shift = 16781614 { $100112e };
    QtKey_Eisu_Shift = 16781615 { $100112f };
    QtKey_Eisu_toggle = 16781616 { $1001130 };
    QtKey_Hangul = 16781617 { $1001131 };
    QtKey_Hangul_Start = 16781618 { $1001132 };
    QtKey_Hangul_End = 16781619 { $1001133 };
    QtKey_Hangul_Hanja = 16781620 { $1001134 };
    QtKey_Hangul_Jamo = 16781621 { $1001135 };
    QtKey_Hangul_Romaja = 16781622 { $1001136 };
    QtKey_Hangul_Jeonja = 16781624 { $1001138 };
    QtKey_Hangul_Banja = 16781625 { $1001139 };
    QtKey_Hangul_PreHanja = 16781626 { $100113a };
    QtKey_Hangul_PostHanja = 16781627 { $100113b };
    QtKey_Hangul_Special = 16781631 { $100113f };
    QtKey_Dead_Grave = 16781904 { $1001250 };
    QtKey_Dead_Acute = 16781905 { $1001251 };
    QtKey_Dead_Circumflex = 16781906 { $1001252 };
    QtKey_Dead_Tilde = 16781907 { $1001253 };
    QtKey_Dead_Macron = 16781908 { $1001254 };
    QtKey_Dead_Breve = 16781909 { $1001255 };
    QtKey_Dead_Abovedot = 16781910 { $1001256 };
    QtKey_Dead_Diaeresis = 16781911 { $1001257 };
    QtKey_Dead_Abovering = 16781912 { $1001258 };
    QtKey_Dead_Doubleacute = 16781913 { $1001259 };
    QtKey_Dead_Caron = 16781914 { $100125a };
    QtKey_Dead_Cedilla = 16781915 { $100125b };
    QtKey_Dead_Ogonek = 16781916 { $100125c };
    QtKey_Dead_Iota = 16781917 { $100125d };
    QtKey_Dead_Voiced_Sound = 16781918 { $100125e };
    QtKey_Dead_Semivoiced_Sound = 16781919 { $100125f };
    QtKey_Dead_Belowdot = 16781920 { $1001260 };
    QtKey_Dead_Hook = 16781921 { $1001261 };
    QtKey_Dead_Horn = 16781922 { $1001262 };
    QtKey_Back = 16777313 { $1000061 };
    QtKey_Forward = 16777314 { $1000062 };
    QtKey_Stop = 16777315 { $1000063 };
    QtKey_Refresh = 16777316 { $1000064 };
    QtKey_VolumeDown = 16777328 { $1000070 };
    QtKey_VolumeMute = 16777329 { $1000071 };
    QtKey_VolumeUp = 16777330 { $1000072 };
    QtKey_BassBoost = 16777331 { $1000073 };
    QtKey_BassUp = 16777332 { $1000074 };
    QtKey_BassDown = 16777333 { $1000075 };
    QtKey_TrebleUp = 16777334 { $1000076 };
    QtKey_TrebleDown = 16777335 { $1000077 };
    QtKey_MediaPlay = 16777344 { $1000080 };
    QtKey_MediaStop = 16777345 { $1000081 };
    QtKey_MediaPrevious = 16777346 { $1000082 };
    QtKey_MediaNext = 16777347 { $1000083 };
    QtKey_MediaRecord = 16777348 { $1000084 };
    QtKey_HomePage = 16777360 { $1000090 };
    QtKey_Favorites = 16777361 { $1000091 };
    QtKey_Search = 16777362 { $1000092 };
    QtKey_Standby = 16777363 { $1000093 };
    QtKey_OpenUrl = 16777364 { $1000094 };
    QtKey_LaunchMail = 16777376 { $10000a0 };
    QtKey_LaunchMedia = 16777377 { $10000a1 };
    QtKey_Launch0 = 16777378 { $10000a2 };
    QtKey_Launch1 = 16777379 { $10000a3 };
    QtKey_Launch2 = 16777380 { $10000a4 };
    QtKey_Launch3 = 16777381 { $10000a5 };
    QtKey_Launch4 = 16777382 { $10000a6 };
    QtKey_Launch5 = 16777383 { $10000a7 };
    QtKey_Launch6 = 16777384 { $10000a8 };
    QtKey_Launch7 = 16777385 { $10000a9 };
    QtKey_Launch8 = 16777386 { $10000aa };
    QtKey_Launch9 = 16777387 { $10000ab };
    QtKey_LaunchA = 16777388 { $10000ac };
    QtKey_LaunchB = 16777389 { $10000ad };
    QtKey_LaunchC = 16777390 { $10000ae };
    QtKey_LaunchD = 16777391 { $10000af };
    QtKey_LaunchE = 16777392 { $10000b0 };
    QtKey_LaunchF = 16777393 { $10000b1 };
    QtKey_MediaLast = 16842751 { $100ffff };
    QtKey_Select = 16842752 { $1010000 };
    QtKey_Yes = 16842753 { $1010001 };
    QtKey_No = 16842754 { $1010002 };
    QtKey_Context1 = 17825792 { $1100000 };
    QtKey_Context2 = 17825793 { $1100001 };
    QtKey_Context3 = 17825794 { $1100002 };
    QtKey_Context4 = 17825795 { $1100003 };
    QtKey_Call = 17825796 { $1100004 };
    QtKey_Hangup = 17825797 { $1100005 };
    QtKey_Flip = 17825798 { $1100006 };
    QtKey_unknown = 33554431 { $1ffffff };

type
  QtCursorShape = cardinal; //  Qt::CursorShape (4)

const
    QtArrowCursor = 0 { $0 };
    QtUpArrowCursor = 1 { $1 };
    QtCrossCursor = 2 { $2 };
    QtWaitCursor = 3 { $3 };
    QtIBeamCursor = 4 { $4 };
    QtSizeVerCursor = 5 { $5 };
    QtSizeHorCursor = 6 { $6 };
    QtSizeBDiagCursor = 7 { $7 };
    QtSizeFDiagCursor = 8 { $8 };
    QtSizeAllCursor = 9 { $9 };
    QtBlankCursor = 10 { $a };
    QtSplitVCursor = 11 { $b };
    QtSplitHCursor = 12 { $c };
    QtPointingHandCursor = 13 { $d };
    QtForbiddenCursor = 14 { $e };
    QtWhatsThisCursor = 15 { $f };
    QtBusyCursor = 16 { $10 };
    QtLastCursor = 16 { $10 };
    QtBitmapCursor = 24 { $18 };
    QtCustomCursor = 25 { $19 };

type
  QtDockWidgetArea = cardinal; //  Qt::DockWidgetArea (4)
  QtDockWidgetAreas = QtDockWidgetArea; // QFlags<>

const
    QtLeftDockWidgetArea = 1 { $1 };
    QtRightDockWidgetArea = 2 { $2 };
    QtTopDockWidgetArea = 4 { $4 };
    QtBottomDockWidgetArea = 8 { $8 };
    QtDockWidgetArea_Mask = 15 { $f };
    QtAllDockWidgetAreas = 15 { $f };

type
  QtToolBarArea = cardinal; //  Qt::ToolBarArea (4)
  QtToolBarAreas = QtToolBarArea; // QFlags<>

const
    QtLeftToolBarArea = 1 { $1 };
    QtRightToolBarArea = 2 { $2 };
    QtTopToolBarArea = 4 { $4 };
    QtBottomToolBarArea = 8 { $8 };
    QtToolBarArea_Mask = 15 { $f };
    QtAllToolBarAreas = 15 { $f };



function QObject_create(parent: QObjectH = nil): QObjectH; cdecl; external QtShareName name QtNamePrefix + 'QObject_create';
procedure QObject_destroy(handle: QObjectH); cdecl; external QtShareName name QtNamePrefix + 'QObject_destroy'; 
function QObject_event(handle: QObjectH; p1: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QObject_event';
function QObject_eventFilter(handle: QObjectH; p1: QObjectH; p2: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QObject_eventFilter';
procedure QObject_objectName(handle: QObjectH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QObject_objectName';
procedure QObject_setObjectName(handle: QObjectH; name: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QObject_setObjectName';
function QObject_isWidgetType(handle: QObjectH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QObject_isWidgetType';
function QObject_signalsBlocked(handle: QObjectH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QObject_signalsBlocked';
function QObject_blockSignals(handle: QObjectH; b: Boolean): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QObject_blockSignals';
function QObject_thread(handle: QObjectH): QThreadH; cdecl; external QtShareName name QtNamePrefix + 'QObject_thread';
procedure QObject_moveToThread(handle: QObjectH; thread: QThreadH); cdecl; external QtShareName name QtNamePrefix + 'QObject_moveToThread';
function QObject_startTimer(handle: QObjectH; interval: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QObject_startTimer';
procedure QObject_killTimer(handle: QObjectH; id: Integer); cdecl; external QtShareName name QtNamePrefix + 'QObject_killTimer';
procedure QObject_setParent(handle: QObjectH; p1: QObjectH); cdecl; external QtShareName name QtNamePrefix + 'QObject_setParent';
procedure QObject_installEventFilter(handle: QObjectH; p1: QObjectH); cdecl; external QtShareName name QtNamePrefix + 'QObject_installEventFilter';
procedure QObject_removeEventFilter(handle: QObjectH; p1: QObjectH); cdecl; external QtShareName name QtNamePrefix + 'QObject_removeEventFilter';
function QObject_connect(sender: QObjectH; signal: PAnsiChar; receiver: QObjectH; member: PAnsiChar; p5: QtConnectionType = QtAutoConnection): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QObject_connect';
function QObject_connect(handle: QObjectH; sender: QObjectH; signal: PAnsiChar; member: PAnsiChar; _type: QtConnectionType = QtAutoConnection): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QObject_connect2';
function QObject_disconnect(sender: QObjectH; signal: PAnsiChar; receiver: QObjectH; member: PAnsiChar): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QObject_disconnect';
function QObject_disconnect(handle: QObjectH; receiver: QObjectH; member: PAnsiChar = 0): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QObject_disconnect3';
procedure QObject_dumpObjectTree(handle: QObjectH); cdecl; external QtShareName name QtNamePrefix + 'QObject_dumpObjectTree';
procedure QObject_dumpObjectInfo(handle: QObjectH); cdecl; external QtShareName name QtNamePrefix + 'QObject_dumpObjectInfo';
function QObject_setProperty(handle: QObjectH; name: PAnsiChar; value: QVariantH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QObject_setProperty';
procedure QObject_property(handle: QObjectH; retval: QVariantH; name: PAnsiChar); cdecl; external QtShareName name QtNamePrefix + 'QObject_property';
function QObject_registerUserData(): Cardinal; cdecl; external QtShareName name QtNamePrefix + 'QObject_registerUserData';
function QObject_parent(handle: QObjectH): QObjectH; cdecl; external QtShareName name QtNamePrefix + 'QObject_parent';
function QObject_inherits(handle: QObjectH; classname: PAnsiChar): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QObject_inherits';
procedure QObject_deleteLater(handle: QObjectH); cdecl; external QtShareName name QtNamePrefix + 'QObject_deleteLater';


type
  QEventType = (  //QEvent::Type (2s)
    QEventNone = 0,
    QEventTimer = 1,
    QEventMouseButtonPress = 2,
    QEventMouseButtonRelease = 3,
    QEventMouseButtonDblClick = 4,
    QEventMouseMove = 5,
    QEventKeyPress = 6,
    QEventKeyRelease = 7,
    QEventFocusIn = 8,
    QEventFocusOut = 9,
    QEventEnter = 10,
    QEventLeave = 11,
    QEventPaint = 12,
    QEventMove = 13,
    QEventResize = 14,
    QEventCreate = 15,
    QEventDestroy = 16,
    QEventShow = 17,
    QEventHide = 18,
    QEventClose = 19,
    QEventQuit = 20,
    QEventParentChange = 21,
    QEventThreadChange = 22,
    QEventWindowActivate = 24,
    QEventWindowDeactivate = 25,
    QEventShowToParent = 26,
    QEventHideToParent = 27,
    QEventWheel = 31,
    QEventWindowTitleChange = 33,
    QEventWindowIconChange = 34,
    QEventApplicationWindowIconChange = 35,
    QEventApplicationFontChange = 36,
    QEventApplicationLayoutDirectionChange = 37,
    QEventApplicationPaletteChange = 38,
    QEventPaletteChange = 39,
    QEventClipboard = 40,
    QEventSpeech = 42,
    QEventMetaCall = 43,
    QEventSockAct = 50,
    QEventShortcutOverride = 51,
    QEventDeferredDelete = 52,
    QEventDragEnter = 60,
    QEventDragMove = 61,
    QEventDragLeave = 62,
    QEventDrop = 63,
    QEventDragResponse = 64,
    QEventChildAdded = 68,
    QEventChildPolished = 69,
    QEventChildRemoved = 71,
    QEventShowWindowRequest = 73,
    QEventPolishRequest = 74,
    QEventPolish = 75,
    QEventLayoutRequest = 76,
    QEventUpdateRequest = 77,
    QEventUpdateLater = 78,
    QEventEmbeddingControl = 79,
    QEventActivateControl = 80,
    QEventDeactivateControl = 81,
    QEventContextMenu = 82,
    QEventInputMethod = 83,
    QEventAccessibilityPrepare = 86,
    QEventTabletMove = 87,
    QEventLocaleChange = 88,
    QEventLanguageChange = 89,
    QEventLayoutDirectionChange = 90,
    QEventStyle = 91,
    QEventTabletPress = 92,
    QEventTabletRelease = 93,
    QEventOkRequest = 94,
    QEventHelpRequest = 95,
    QEventIconDrag = 96,
    QEventFontChange = 97,
    QEventEnabledChange = 98,
    QEventActivationChange = 99,
    QEventStyleChange = 100,
    QEventIconTextChange = 101,
    QEventModifiedChange = 102,
    QEventWindowBlocked = 103,
    QEventWindowUnblocked = 104,
    QEventWindowStateChange = 105,
    QEventMouseTrackingChange = 109,
    QEventToolTip = 110,
    QEventWhatsThis = 111,
    QEventStatusTip = 112,
    QEventActionChanged = 113,
    QEventActionAdded = 114,
    QEventActionRemoved = 115,
    QEventFileOpen = 116,
    QEventShortcut = 117,
    QEventWhatsThisClicked = 118,
    QEventAccessibilityHelp = 119,
    QEventToolBarChange = 120,
    QEventApplicationActivated = 121,
    QEventApplicationDeactivated = 122,
    QEventQueryWhatsThis = 123,
    QEventEnterWhatsThisMode = 124,
    QEventLeaveWhatsThisMode = 125,
    QEventZOrderChange = 126,
    QEventHoverEnter = 127,
    QEventHoverLeave = 128,
    QEventHoverMove = 129,
    QEventAccessibilityDescription = 130,
    QEventParentAboutToChange = 131,
    QEventWinEventAct = 132,
    QEventAcceptDropsChange = 152,
    QEventMenubarUpdated = 153,
    QEventZeroTimerEvent = 154,
    QEventUser = 1000,
    QEventMaxUser = 65535 );

function QEvent_create(_type: QEventType): QEventH; cdecl; external QtShareName name QtNamePrefix + 'QEvent_create';
procedure QEvent_destroy(handle: QEventH); cdecl; external QtShareName name QtNamePrefix + 'QEvent_destroy'; 
function QEvent_type(handle: QEventH): QEventType; cdecl; external QtShareName name QtNamePrefix + 'QEvent_type';
function QEvent_spontaneous(handle: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QEvent_spontaneous';
procedure QEvent_setAccepted(handle: QEventH; accepted: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QEvent_setAccepted';
function QEvent_isAccepted(handle: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QEvent_isAccepted';
procedure QEvent_accept(handle: QEventH); cdecl; external QtShareName name QtNamePrefix + 'QEvent_accept';
procedure QEvent_ignore(handle: QEventH); cdecl; external QtShareName name QtNamePrefix + 'QEvent_ignore';

function QTimerEvent_create(timerId: Integer): QTimerEventH; cdecl; external QtShareName name QtNamePrefix + 'QTimerEvent_create';
procedure QTimerEvent_destroy(handle: QTimerEventH); cdecl; external QtShareName name QtNamePrefix + 'QTimerEvent_destroy'; 
function QTimerEvent_timerId(handle: QTimerEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTimerEvent_timerId';

function QChildEvent_create(_type: QEventType; child: QObjectH): QChildEventH; cdecl; external QtShareName name QtNamePrefix + 'QChildEvent_create';
procedure QChildEvent_destroy(handle: QChildEventH); cdecl; external QtShareName name QtNamePrefix + 'QChildEvent_destroy'; 
function QChildEvent_child(handle: QChildEventH): QObjectH; cdecl; external QtShareName name QtNamePrefix + 'QChildEvent_child';
function QChildEvent_added(handle: QChildEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QChildEvent_added';
function QChildEvent_polished(handle: QChildEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QChildEvent_polished';
function QChildEvent_removed(handle: QChildEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QChildEvent_removed';

type
  QEventLoopProcessEventsFlag = cardinal; // QEventLoop::ProcessEventsFlag
  QEventLoopProcessEventsFlags = QEventLoopProcessEventsFlag; //QFlags<> (3)
const
  QEventLoopAllEvents =   $00;
  QEventLoopExcludeUserInputEvents =   $01;
  QEventLoopExcludeSocketNotifiers =   $02;
  QEventLoopWaitForMoreEvents =   $04;
  QEventLoopX11ExcludeTimers =   $08;
  QEventLoopDeferredDeletion =   $10;

function QEventLoop_create(parent: QObjectH = nil): QEventLoopH; cdecl; external QtShareName name QtNamePrefix + 'QEventLoop_create';
procedure QEventLoop_destroy(handle: QEventLoopH); cdecl; external QtShareName name QtNamePrefix + 'QEventLoop_destroy'; 
function QEventLoop_processEvents(handle: QEventLoopH; flags: QEventLoopProcessEventsFlags = QEventLoopAllEvents): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QEventLoop_processEvents';
procedure QEventLoop_processEvents(handle: QEventLoopH; flags: QEventLoopProcessEventsFlags; maximumTime: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QEventLoop_processEvents2';
function QEventLoop_exec(handle: QEventLoopH; flags: QEventLoopProcessEventsFlags = QEventLoopAllEvents): Integer; cdecl; external QtShareName name QtNamePrefix + 'QEventLoop_exec';
procedure QEventLoop_exit(handle: QEventLoopH; returnCode: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QEventLoop_exit';
function QEventLoop_isRunning(handle: QEventLoopH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QEventLoop_isRunning';
procedure QEventLoop_wakeUp(handle: QEventLoopH); cdecl; external QtShareName name QtNamePrefix + 'QEventLoop_wakeUp';
procedure QEventLoop_quit(handle: QEventLoopH); cdecl; external QtShareName name QtNamePrefix + 'QEventLoop_quit';


type
  QCoreApplicationEncoding = ( // QCoreApplication::Encoding (1)
    QCoreApplicationDefaultCodec, QCoreApplicationUnicodeUTF8 );

function QCoreApplication_create(argc: PInteger; argv: PPAnsiChar): QCoreApplicationH; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_create';
procedure QCoreApplication_destroy(handle: QCoreApplicationH); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_destroy'; 
function QCoreApplication_argc(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_argc';
function QCoreApplication_argv(): PPAnsiChar; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_argv';
procedure QCoreApplication_arguments(retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_arguments';
procedure QCoreApplication_setOrganizationDomain(orgDomain: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_setOrganizationDomain';
procedure QCoreApplication_organizationDomain(retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_organizationDomain';
procedure QCoreApplication_setOrganizationName(orgName: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_setOrganizationName';
procedure QCoreApplication_organizationName(retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_organizationName';
procedure QCoreApplication_setApplicationName(application: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_setApplicationName';
procedure QCoreApplication_applicationName(retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_applicationName';
function QCoreApplication_instance(): QCoreApplicationH; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_instance';
function QCoreApplication_exec(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_exec';
procedure QCoreApplication_processEvents(flags: QEventLoopProcessEventsFlags = QEventLoopAllEvents); overload; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_processEvents';
procedure QCoreApplication_processEvents(flags: QEventLoopProcessEventsFlags; maxtime: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_processEvents2';
procedure QCoreApplication_exit(retcode: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_exit';
function QCoreApplication_sendEvent(receiver: QObjectH; event: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_sendEvent';
procedure QCoreApplication_postEvent(receiver: QObjectH; event: QEventH); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_postEvent';
procedure QCoreApplication_sendPostedEvents(receiver: QObjectH; event_type: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_sendPostedEvents';
procedure QCoreApplication_sendPostedEvents(); overload; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_sendPostedEvents2';
procedure QCoreApplication_removePostedEvents(receiver: QObjectH); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_removePostedEvents';
function QCoreApplication_hasPendingEvents(): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_hasPendingEvents';
function QCoreApplication_notify(handle: QCoreApplicationH; p1: QObjectH; p2: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_notify';
function QCoreApplication_startingUp(): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_startingUp';
function QCoreApplication_closingDown(): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_closingDown';
procedure QCoreApplication_applicationDirPath(retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_applicationDirPath';
procedure QCoreApplication_applicationFilePath(retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_applicationFilePath';
procedure QCoreApplication_setLibraryPaths(p1: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_setLibraryPaths';
procedure QCoreApplication_libraryPaths(retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_libraryPaths';
procedure QCoreApplication_addLibraryPath(p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_addLibraryPath';
procedure QCoreApplication_removeLibraryPath(p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_removeLibraryPath';
procedure QCoreApplication_installTranslator(messageFile: QTranslatorH); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_installTranslator';
procedure QCoreApplication_removeTranslator(messageFile: QTranslatorH); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_removeTranslator';
procedure QCoreApplication_translate(retval: PWideString; context: PAnsiChar; key: PAnsiChar; comment: PAnsiChar = 0; encoding: QCoreApplicationEncoding = QCoreApplicationDefaultCodec); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_translate';
procedure QCoreApplication_flush(); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_flush';
function QCoreApplication_winEventFilter(handle: QCoreApplicationH; message: PMsg; _result: PInteger): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_winEventFilter';
function QCoreApplication_setEventFilter(handle: QCoreApplicationH; filter: TCoreApplicationEventFilter): TCoreApplicationEventFilter; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_setEventFilter';
function QCoreApplication_filterEvent(handle: QCoreApplicationH; message: Pointer; _result: PInteger): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_filterEvent';
procedure QCoreApplication_quit(); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_quit';


type
  QCoreApplication_aboutToQuit_Event = procedure () of object cdecl;
  QCoreApplication_unixSignal_Event = procedure (p1: Integer) of object cdecl;


function QTranslator_create(parent: QObjectH = nil): QTranslatorH; cdecl; external QtShareName name QtNamePrefix + 'QTranslator_create';
procedure QTranslator_destroy(handle: QTranslatorH); cdecl; external QtShareName name QtNamePrefix + 'QTranslator_destroy'; 
procedure QTranslator_translate(handle: QTranslatorH; retval: PWideString; context: PAnsiChar; sourceText: PAnsiChar; comment: PAnsiChar = 0); cdecl; external QtShareName name QtNamePrefix + 'QTranslator_translate';
function QTranslator_isEmpty(handle: QTranslatorH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTranslator_isEmpty';
function QTranslator_load(handle: QTranslatorH; filename: PWideString; directory: PWideString = nil; search_delimiters: PWideString = nil; suffix: PWideString = nil): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QTranslator_load';
function QTranslator_load(handle: QTranslatorH; data: PByte; len: Integer): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QTranslator_load2';

function QTimer_create(parent: QObjectH = nil): QTimerH; cdecl; external QtShareName name QtNamePrefix + 'QTimer_create';
procedure QTimer_destroy(handle: QTimerH); cdecl; external QtShareName name QtNamePrefix + 'QTimer_destroy'; 
function QTimer_isActive(handle: QTimerH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTimer_isActive';
function QTimer_timerId(handle: QTimerH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTimer_timerId';
procedure QTimer_setInterval(handle: QTimerH; msec: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTimer_setInterval';
function QTimer_interval(handle: QTimerH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTimer_interval';
procedure QTimer_setSingleShot(handle: QTimerH; singleShot: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTimer_setSingleShot';
function QTimer_isSingleShot(handle: QTimerH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTimer_isSingleShot';
procedure QTimer_singleShot(msec: Integer; receiver: QObjectH; member: PAnsiChar); cdecl; external QtShareName name QtNamePrefix + 'QTimer_singleShot';
procedure QTimer_start(handle: QTimerH; msec: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QTimer_start';
procedure QTimer_start(handle: QTimerH); overload; cdecl; external QtShareName name QtNamePrefix + 'QTimer_start2';
procedure QTimer_stop(handle: QTimerH); cdecl; external QtShareName name QtNamePrefix + 'QTimer_stop';


type
  QTimer_timeout_Event = procedure () of object cdecl;



type
  QVariantType = (  //QVariant::Type (2s)
    QVariantInvalid = 0,
    QVariantBool = 1,
    QVariantInt = 2,
    QVariantUInt = 3,
    QVariantLongLong = 4,
    QVariantULongLong = 5,
    QVariantDouble = 6,
    QVariantChar = 7,
    QVariantMap = 8,
    QVariantList = 9,
    QVariantString = 10,
    QVariantStringList = 11,
    QVariantByteArray = 12,
    QVariantBitArray = 13,
    QVariantDate = 14,
    QVariantTime = 15,
    QVariantDateTime = 16,
    QVariantUrl = 17,
    QVariantLocale = 18,
    QVariantRect = 19,
    QVariantRectF = 20,
    QVariantSize = 21,
    QVariantSizeF = 22,
    QVariantLine = 23,
    QVariantLineF = 24,
    QVariantPoint = 25,
    QVariantPointF = 26,
    QVariantRegExp = 27,
    QVariantFont = 64,
    QVariantPixmap = 65,
    QVariantBrush = 66,
    QVariantColor = 67,
    QVariantPalette = 68,
    QVariantIcon = 69,
    QVariantImage = 70,
    QVariantPolygon = 71,
    QVariantRegion = 72,
    QVariantBitmap = 73,
    QVariantCursor = 74,
    QVariantSizePolicy = 75,
    QVariantKeySequence = 76,
    QVariantPen = 77,
    QVariantTextLength = 78,
    QVariantTextFormat = 79,
    QVariantUserType = 127,
    QVariantLastType = $ffffffff );

function QVariant_create(): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create';
procedure QVariant_destroy(handle: QVariantH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_destroy'; 
function QVariant_create(_type: QVariantType): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create2';
function QVariant_create(typeOrUserType: Integer; copy: Pointer): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create3';
function QVariant_create(other: QVariantH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create4';
function QVariant_create(s: QDataStreamH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create5';
function QVariant_create(i: Integer): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create6';
function QVariant_create(ui: Cardinal): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create7';
function QVariant_create(ll: int64): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create8';
function QVariant_create(ull: qword): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create9';
function QVariant_create(b: Boolean): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create10';
function QVariant_create(d: Double): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create11';
function QVariant_create(str: PAnsiChar): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create12';
function QVariant_create(bytearray: QByteArrayH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create13';
function QVariant_create(bitarray: QBitArrayH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create14';
function QVariant_create(_string: PWideString): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create15';
function QVariant_create(_string: QLatin1StringH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create16';
function QVariant_create(stringlist: QStringListH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create17';
function QVariant_create(qchar: PWideChar): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create18';
function QVariant_create(date: QDateH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create19';
function QVariant_create(time: QTimeH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create20';
function QVariant_create(datetime: QDateTimeH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create21';
function QVariant_create(size: PSize): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create24';
function QVariant_create(size: QSizeFH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create25';
function QVariant_create(pt: PPoint): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create26';
function QVariant_create(pt: QPointFH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create27';
function QVariant_create(line: QLineH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create28';
function QVariant_create(line: QLineFH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create29';
function QVariant_create(rect: PRect): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create30';
function QVariant_create(rect: QRectFH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create31';
function QVariant_create(url: QUrlH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create32';
function QVariant_create(locale: QLocaleH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create33';
function QVariant_create(regExp: QRegExpH): QVariantH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_create34';
function QVariant_type(handle: QVariantH): QVariantType; cdecl; external QtShareName name QtNamePrefix + 'QVariant_type';
function QVariant_userType(handle: QVariantH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QVariant_userType';
function QVariant_typeName(handle: QVariantH): PAnsiChar; cdecl; external QtShareName name QtNamePrefix + 'QVariant_typeName';
function QVariant_canConvert(handle: QVariantH; t: QVariantType): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QVariant_canConvert';
function QVariant_convert(handle: QVariantH; t: QVariantType): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QVariant_convert';
function QVariant_isValid(handle: QVariantH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QVariant_isValid';
function QVariant_isNull(handle: QVariantH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QVariant_isNull';
procedure QVariant_clear(handle: QVariantH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_clear';
procedure QVariant_detach(handle: QVariantH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_detach';
function QVariant_isDetached(handle: QVariantH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QVariant_isDetached';
function QVariant_toInt(handle: QVariantH; ok: PBoolean = 0): Integer; cdecl; external QtShareName name QtNamePrefix + 'QVariant_toInt';
function QVariant_toUInt(handle: QVariantH; ok: PBoolean = 0): Cardinal; cdecl; external QtShareName name QtNamePrefix + 'QVariant_toUInt';
function QVariant_toLongLong(handle: QVariantH; ok: PBoolean = 0): int64; cdecl; external QtShareName name QtNamePrefix + 'QVariant_toLongLong';
function QVariant_toULongLong(handle: QVariantH; ok: PBoolean = 0): qword; cdecl; external QtShareName name QtNamePrefix + 'QVariant_toULongLong';
function QVariant_toBool(handle: QVariantH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QVariant_toBool';
function QVariant_toDouble(handle: QVariantH; ok: PBoolean = 0): Double; cdecl; external QtShareName name QtNamePrefix + 'QVariant_toDouble';
procedure QVariant_toByteArray(handle: QVariantH; retval: QByteArrayH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toByteArray';
procedure QVariant_toBitArray(handle: QVariantH; retval: QBitArrayH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toBitArray';
procedure QVariant_toString(handle: QVariantH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toString';
procedure QVariant_toStringList(handle: QVariantH; retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toStringList';
procedure QVariant_toChar(handle: QVariantH; retval: PWideChar); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toChar';
procedure QVariant_toDate(handle: QVariantH; retval: QDateH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toDate';
procedure QVariant_toTime(handle: QVariantH; retval: QTimeH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toTime';
procedure QVariant_toDateTime(handle: QVariantH; retval: QDateTimeH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toDateTime';
procedure QVariant_toPoint(handle: QVariantH; retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toPoint';
procedure QVariant_toPointF(handle: QVariantH; retval: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toPointF';
procedure QVariant_toRect(handle: QVariantH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toRect';
procedure QVariant_toSize(handle: QVariantH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toSize';
procedure QVariant_toSizeF(handle: QVariantH; retval: QSizeFH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toSizeF';
procedure QVariant_toLine(handle: QVariantH; retval: QLineH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toLine';
procedure QVariant_toLineF(handle: QVariantH; retval: QLineFH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toLineF';
procedure QVariant_toRectF(handle: QVariantH; retval: QRectFH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toRectF';
procedure QVariant_toUrl(handle: QVariantH; retval: QUrlH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toUrl';
procedure QVariant_toLocale(handle: QVariantH; retval: QLocaleH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toLocale';
procedure QVariant_toRegExp(handle: QVariantH; retval: QRegExpH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_toRegExp';
procedure QVariant_load(handle: QVariantH; ds: QDataStreamH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_load';
procedure QVariant_save(handle: QVariantH; ds: QDataStreamH); cdecl; external QtShareName name QtNamePrefix + 'QVariant_save';
function QVariant_typeToName(_type: QVariantType): PAnsiChar; cdecl; external QtShareName name QtNamePrefix + 'QVariant_typeToName';
function QVariant_nameToType(name: PAnsiChar): QVariantType; cdecl; external QtShareName name QtNamePrefix + 'QVariant_nameToType';
function QVariant_constData(handle: QVariantH): Pointer; cdecl; external QtShareName name QtNamePrefix + 'QVariant_constData';

function QStringList_create(): QStringListH; overload; cdecl; external QtShareName name QtNamePrefix + 'QStringList_create';
procedure QStringList_destroy(handle: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QStringList_destroy'; 
function QStringList_create(i: PWideString): QStringListH; overload; cdecl; external QtShareName name QtNamePrefix + 'QStringList_create2';
function QStringList_create(l: QStringListH): QStringListH; overload; cdecl; external QtShareName name QtNamePrefix + 'QStringList_create3';
procedure QStringList_sort(handle: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QStringList_sort';
procedure QStringList_join(handle: QStringListH; retval: PWideString; sep: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QStringList_join';
procedure QStringList_filter(handle: QStringListH; retval: QStringListH; str: PWideString; cs: QtCaseSensitivity = QtCaseSensitive); cdecl; external QtShareName name QtNamePrefix + 'QStringList_filter';
function QStringList_contains(handle: QStringListH; str: PWideString; cs: QtCaseSensitivity = QtCaseSensitive): boolean; cdecl; external QtShareName name QtNamePrefix + 'QStringList_contains';
function QStringList_replaceInStrings(handle: QStringListH; before: PWideString; after: PWideString; cs: QtCaseSensitivity = QtCaseSensitive): QStringListH; overload; cdecl; external QtShareName name QtNamePrefix + 'QStringList_replaceInStrings';
function QStringList_replaceInStrings(handle: QStringListH; rx: QRegExpH; after: PWideString): QStringListH; overload; cdecl; external QtShareName name QtNamePrefix + 'QStringList_replaceInStrings2';
function QStringList_indexOf(handle: QStringListH; rx: QRegExpH; from: Integer = 0): Integer; cdecl; external QtShareName name QtNamePrefix + 'QStringList_indexOf';
function QStringList_lastIndexOf(handle: QStringListH; rx: QRegExpH; from: Integer = -1): Integer; cdecl; external QtShareName name QtNamePrefix + 'QStringList_lastIndexOf';

function QRect_create(): QRectH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_create';
procedure QRect_destroy(handle: QRectH); cdecl; external QtShareName name QtNamePrefix + 'QRect_destroy'; 
function QRect_create(topleft: PPoint; bottomright: PPoint): QRectH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_create2';
function QRect_create(topleft: PPoint; size: PSize): QRectH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_create3';
function QRect_create(left: Integer; top: Integer; width: Integer; height: Integer): QRectH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_create4';
function QRect_isNull(handle: QRectH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QRect_isNull';
function QRect_isEmpty(handle: QRectH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QRect_isEmpty';
function QRect_isValid(handle: QRectH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QRect_isValid';
function QRect_left(handle: QRectH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QRect_left';
function QRect_top(handle: QRectH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QRect_top';
function QRect_right(handle: QRectH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QRect_right';
function QRect_bottom(handle: QRectH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QRect_bottom';
procedure QRect_normalized(handle: QRectH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QRect_normalized';
function QRect_x(handle: QRectH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QRect_x';
function QRect_y(handle: QRectH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QRect_y';
procedure QRect_setLeft(handle: QRectH; pos: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_setLeft';
procedure QRect_setTop(handle: QRectH; pos: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_setTop';
procedure QRect_setRight(handle: QRectH; pos: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_setRight';
procedure QRect_setBottom(handle: QRectH; pos: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_setBottom';
procedure QRect_setX(handle: QRectH; x: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_setX';
procedure QRect_setY(handle: QRectH; y: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_setY';
procedure QRect_setTopLeft(handle: QRectH; p: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_setTopLeft';
procedure QRect_setBottomRight(handle: QRectH; p: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_setBottomRight';
procedure QRect_setTopRight(handle: QRectH; p: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_setTopRight';
procedure QRect_setBottomLeft(handle: QRectH; p: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_setBottomLeft';
procedure QRect_topLeft(handle: QRectH; retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_topLeft';
procedure QRect_bottomRight(handle: QRectH; retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_bottomRight';
procedure QRect_topRight(handle: QRectH; retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_topRight';
procedure QRect_bottomLeft(handle: QRectH; retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_bottomLeft';
procedure QRect_center(handle: QRectH; retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_center';
procedure QRect_moveLeft(handle: QRectH; pos: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_moveLeft';
procedure QRect_moveTop(handle: QRectH; pos: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_moveTop';
procedure QRect_moveRight(handle: QRectH; pos: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_moveRight';
procedure QRect_moveBottom(handle: QRectH; pos: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_moveBottom';
procedure QRect_moveTopLeft(handle: QRectH; p: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_moveTopLeft';
procedure QRect_moveBottomRight(handle: QRectH; p: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_moveBottomRight';
procedure QRect_moveTopRight(handle: QRectH; p: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_moveTopRight';
procedure QRect_moveBottomLeft(handle: QRectH; p: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_moveBottomLeft';
procedure QRect_moveCenter(handle: QRectH; p: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QRect_moveCenter';
procedure QRect_translate(handle: QRectH; dx: Integer; dy: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_translate';
procedure QRect_translate(handle: QRectH; p: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_translate2';
procedure QRect_translated(handle: QRectH; retval: PRect; dx: Integer; dy: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_translated';
procedure QRect_translated(handle: QRectH; retval: PRect; p: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_translated2';
procedure QRect_moveTo(handle: QRectH; x: Integer; t: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_moveTo';
procedure QRect_moveTo(handle: QRectH; p: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_moveTo2';
procedure QRect_setRect(handle: QRectH; x: Integer; y: Integer; w: Integer; h: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_setRect';
procedure QRect_getRect(handle: QRectH; x: PInteger; y: PInteger; w: PInteger; h: PInteger); cdecl; external QtShareName name QtNamePrefix + 'QRect_getRect';
procedure QRect_setCoords(handle: QRectH; x1: Integer; y1: Integer; x2: Integer; y2: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_setCoords';
procedure QRect_getCoords(handle: QRectH; x1: PInteger; y1: PInteger; x2: PInteger; y2: PInteger); cdecl; external QtShareName name QtNamePrefix + 'QRect_getCoords';
procedure QRect_adjust(handle: QRectH; x1: Integer; y1: Integer; x2: Integer; y2: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_adjust';
procedure QRect_adjusted(handle: QRectH; retval: PRect; x1: Integer; y1: Integer; x2: Integer; y2: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_adjusted';
procedure QRect_size(handle: QRectH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QRect_size';
function QRect_width(handle: QRectH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QRect_width';
function QRect_height(handle: QRectH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QRect_height';
procedure QRect_setWidth(handle: QRectH; w: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_setWidth';
procedure QRect_setHeight(handle: QRectH; h: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRect_setHeight';
procedure QRect_setSize(handle: QRectH; s: PSize); cdecl; external QtShareName name QtNamePrefix + 'QRect_setSize';
function QRect_contains(handle: QRectH; p: PPoint; proper: Boolean = False): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_contains';
function QRect_contains(handle: QRectH; x: Integer; y: Integer): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_contains2';
function QRect_contains(handle: QRectH; x: Integer; y: Integer; proper: Boolean): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_contains3';
function QRect_contains(handle: QRectH; r: PRect; proper: Boolean = False): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QRect_contains4';
procedure QRect_unite(handle: QRectH; retval: PRect; r: PRect); cdecl; external QtShareName name QtNamePrefix + 'QRect_unite';
procedure QRect_intersect(handle: QRectH; retval: PRect; r: PRect); cdecl; external QtShareName name QtNamePrefix + 'QRect_intersect';
function QRect_intersects(handle: QRectH; r: PRect): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QRect_intersects';

function QRectF_create(): QRectFH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_create';
procedure QRectF_destroy(handle: QRectFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_destroy'; 
function QRectF_create(topleft: QPointFH; size: QSizeFH): QRectFH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_create2';
function QRectF_create(left: Double; top: Double; width: Double; height: Double): QRectFH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_create3';
function QRectF_create(rect: PRect): QRectFH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_create4';
function QRectF_isNull(handle: QRectFH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QRectF_isNull';
function QRectF_isEmpty(handle: QRectFH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QRectF_isEmpty';
function QRectF_isValid(handle: QRectFH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QRectF_isValid';
procedure QRectF_normalized(handle: QRectFH; retval: QRectFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_normalized';
function QRectF_left(handle: QRectFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QRectF_left';
function QRectF_top(handle: QRectFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QRectF_top';
function QRectF_right(handle: QRectFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QRectF_right';
function QRectF_bottom(handle: QRectFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QRectF_bottom';
function QRectF_x(handle: QRectFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QRectF_x';
function QRectF_y(handle: QRectFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QRectF_y';
procedure QRectF_setLeft(handle: QRectFH; pos: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setLeft';
procedure QRectF_setTop(handle: QRectFH; pos: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setTop';
procedure QRectF_setRight(handle: QRectFH; pos: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setRight';
procedure QRectF_setBottom(handle: QRectFH; pos: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setBottom';
procedure QRectF_setX(handle: QRectFH; pos: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setX';
procedure QRectF_setY(handle: QRectFH; pos: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setY';
procedure QRectF_topLeft(handle: QRectFH; retval: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_topLeft';
procedure QRectF_bottomRight(handle: QRectFH; retval: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_bottomRight';
procedure QRectF_topRight(handle: QRectFH; retval: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_topRight';
procedure QRectF_bottomLeft(handle: QRectFH; retval: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_bottomLeft';
procedure QRectF_center(handle: QRectFH; retval: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_center';
procedure QRectF_setTopLeft(handle: QRectFH; p: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setTopLeft';
procedure QRectF_setBottomRight(handle: QRectFH; p: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setBottomRight';
procedure QRectF_setTopRight(handle: QRectFH; p: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setTopRight';
procedure QRectF_setBottomLeft(handle: QRectFH; p: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setBottomLeft';
procedure QRectF_moveLeft(handle: QRectFH; pos: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_moveLeft';
procedure QRectF_moveTop(handle: QRectFH; pos: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_moveTop';
procedure QRectF_moveRight(handle: QRectFH; pos: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_moveRight';
procedure QRectF_moveBottom(handle: QRectFH; pos: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_moveBottom';
procedure QRectF_moveTopLeft(handle: QRectFH; p: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_moveTopLeft';
procedure QRectF_moveBottomRight(handle: QRectFH; p: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_moveBottomRight';
procedure QRectF_moveTopRight(handle: QRectFH; p: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_moveTopRight';
procedure QRectF_moveBottomLeft(handle: QRectFH; p: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_moveBottomLeft';
procedure QRectF_moveCenter(handle: QRectFH; p: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_moveCenter';
procedure QRectF_translate(handle: QRectFH; dx: Double; dy: Double); overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_translate';
procedure QRectF_translate(handle: QRectFH; p: QPointFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_translate2';
procedure QRectF_translated(handle: QRectFH; retval: QRectFH; dx: Double; dy: Double); overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_translated';
procedure QRectF_translated(handle: QRectFH; retval: QRectFH; p: QPointFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_translated2';
procedure QRectF_moveTo(handle: QRectFH; x: Double; t: Double); overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_moveTo';
procedure QRectF_moveTo(handle: QRectFH; p: QPointFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_moveTo2';
procedure QRectF_setRect(handle: QRectFH; x: Double; y: Double; w: Double; h: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setRect';
procedure QRectF_getRect(handle: QRectFH; x: PDouble; y: PDouble; w: PDouble; h: PDouble); cdecl; external QtShareName name QtNamePrefix + 'QRectF_getRect';
procedure QRectF_setCoords(handle: QRectFH; x1: Double; y1: Double; x2: Double; y2: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setCoords';
procedure QRectF_getCoords(handle: QRectFH; x1: PDouble; y1: PDouble; x2: PDouble; y2: PDouble); cdecl; external QtShareName name QtNamePrefix + 'QRectF_getCoords';
procedure QRectF_adjust(handle: QRectFH; x1: Double; y1: Double; x2: Double; y2: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_adjust';
procedure QRectF_adjusted(handle: QRectFH; retval: QRectFH; x1: Double; y1: Double; x2: Double; y2: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_adjusted';
procedure QRectF_size(handle: QRectFH; retval: QSizeFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_size';
function QRectF_width(handle: QRectFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QRectF_width';
function QRectF_height(handle: QRectFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QRectF_height';
procedure QRectF_setWidth(handle: QRectFH; w: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setWidth';
procedure QRectF_setHeight(handle: QRectFH; h: Double); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setHeight';
procedure QRectF_setSize(handle: QRectFH; s: QSizeFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_setSize';
function QRectF_contains(handle: QRectFH; p: QPointFH): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_contains';
function QRectF_contains(handle: QRectFH; x: Double; y: Double): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_contains2';
function QRectF_contains(handle: QRectFH; r: QRectFH): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QRectF_contains3';
procedure QRectF_unite(handle: QRectFH; retval: QRectFH; r: QRectFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_unite';
procedure QRectF_intersect(handle: QRectFH; retval: QRectFH; r: QRectFH); cdecl; external QtShareName name QtNamePrefix + 'QRectF_intersect';
function QRectF_intersects(handle: QRectFH; r: QRectFH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QRectF_intersects';
procedure QRectF_toRect(handle: QRectFH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QRectF_toRect';


type
  QApplicationType = ( // QApplication::Type (1)
    QApplicationTty, QApplicationGuiClient, QApplicationGuiServer );

  QApplicationColorSpec = (  //QApplication::ColorSpec (2s)
    QApplicationNormalColor = 0,
    QApplicationCustomColor = 1,
    QApplicationManyColor = 2 );

function QApplication_create(argc: PInteger; argv: PPAnsiChar): QApplicationH; overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_create';
procedure QApplication_destroy(handle: QApplicationH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_destroy'; 
function QApplication_create(argc: PInteger; argv: PPAnsiChar; GUIenabled: Boolean): QApplicationH; overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_create2';
function QApplication_create(argc: PInteger; argv: PPAnsiChar; p3: QApplicationType): QApplicationH; overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_create3';
function QApplication_type(): QApplicationType; cdecl; external QtShareName name QtNamePrefix + 'QApplication_type';
function QApplication_style(): QStyleH; cdecl; external QtShareName name QtNamePrefix + 'QApplication_style';
procedure QApplication_setStyle(p1: QStyleH); overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_setStyle';
function QApplication_setStyle(p1: PWideString): QStyleH; overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_setStyle2';
function QApplication_colorSpec(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QApplication_colorSpec';
procedure QApplication_setColorSpec(p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setColorSpec';
function QApplication_overrideCursor(): QCursorH; cdecl; external QtShareName name QtNamePrefix + 'QApplication_overrideCursor';
procedure QApplication_setOverrideCursor(p1: QCursorH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setOverrideCursor';
procedure QApplication_changeOverrideCursor(p1: QCursorH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_changeOverrideCursor';
procedure QApplication_restoreOverrideCursor(); cdecl; external QtShareName name QtNamePrefix + 'QApplication_restoreOverrideCursor';
procedure QApplication_palette(retval: QPaletteH); overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_palette';
procedure QApplication_palette(retval: QPaletteH; p1: QWidgetH); overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_palette2';
procedure QApplication_palette(retval: QPaletteH; className: PAnsiChar); overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_palette3';
procedure QApplication_setPalette(p1: QPaletteH; className: PAnsiChar = 0); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setPalette';
procedure QApplication_font(retval: QFontH; p1: QWidgetH = nil); cdecl; external QtShareName name QtNamePrefix + 'QApplication_font';
procedure QApplication_setFont(p1: QFontH; className: PAnsiChar = 0); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setFont';
procedure QApplication_fontMetrics(retval: QFontMetricsH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_fontMetrics';
procedure QApplication_setWindowIcon(icon: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setWindowIcon';
procedure QApplication_windowIcon(retval: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_windowIcon';
function QApplication_desktop(): QDesktopWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QApplication_desktop';
function QApplication_activePopupWidget(): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QApplication_activePopupWidget';
function QApplication_activeModalWidget(): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QApplication_activeModalWidget';
function QApplication_clipboard(): QClipboardH; cdecl; external QtShareName name QtNamePrefix + 'QApplication_clipboard';
function QApplication_focusWidget(): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QApplication_focusWidget';
function QApplication_activeWindow(): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QApplication_activeWindow';
procedure QApplication_setActiveWindow(act: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setActiveWindow';
function QApplication_widgetAt(p: PPoint): QWidgetH; overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_widgetAt';
function QApplication_widgetAt(x: Integer; y: Integer): QWidgetH; overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_widgetAt2';
function QApplication_topLevelAt(p: PPoint): QWidgetH; overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_topLevelAt';
function QApplication_topLevelAt(x: Integer; y: Integer): QWidgetH; overload; cdecl; external QtShareName name QtNamePrefix + 'QApplication_topLevelAt2';
procedure QApplication_syncX(); cdecl; external QtShareName name QtNamePrefix + 'QApplication_syncX';
procedure QApplication_beep(); cdecl; external QtShareName name QtNamePrefix + 'QApplication_beep';
function QApplication_keyboardModifiers(): QtKeyboardModifiers; cdecl; external QtShareName name QtNamePrefix + 'QApplication_keyboardModifiers';
function QApplication_mouseButtons(): QtMouseButtons; cdecl; external QtShareName name QtNamePrefix + 'QApplication_mouseButtons';
procedure QApplication_setDesktopSettingsAware(p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setDesktopSettingsAware';
function QApplication_desktopSettingsAware(): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QApplication_desktopSettingsAware';
procedure QApplication_setCursorFlashTime(p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setCursorFlashTime';
function QApplication_cursorFlashTime(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QApplication_cursorFlashTime';
procedure QApplication_setDoubleClickInterval(p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setDoubleClickInterval';
function QApplication_doubleClickInterval(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QApplication_doubleClickInterval';
procedure QApplication_setKeyboardInputInterval(p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setKeyboardInputInterval';
function QApplication_keyboardInputInterval(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QApplication_keyboardInputInterval';
procedure QApplication_setWheelScrollLines(p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setWheelScrollLines';
function QApplication_wheelScrollLines(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QApplication_wheelScrollLines';
procedure QApplication_setGlobalStrut(p1: PSize); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setGlobalStrut';
procedure QApplication_globalStrut(retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QApplication_globalStrut';
procedure QApplication_setStartDragTime(ms: Integer); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setStartDragTime';
function QApplication_startDragTime(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QApplication_startDragTime';
procedure QApplication_setStartDragDistance(l: Integer); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setStartDragDistance';
function QApplication_startDragDistance(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QApplication_startDragDistance';
procedure QApplication_setLayoutDirection(direction: QtLayoutDirection); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setLayoutDirection';
function QApplication_layoutDirection(): QtLayoutDirection; cdecl; external QtShareName name QtNamePrefix + 'QApplication_layoutDirection';
function QApplication_isRightToLeft(): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QApplication_isRightToLeft';
function QApplication_isLeftToRight(): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QApplication_isLeftToRight';
function QApplication_isEffectEnabled(p1: QtUIEffect): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QApplication_isEffectEnabled';
procedure QApplication_setEffectEnabled(p1: QtUIEffect; enable: Boolean = True); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setEffectEnabled';
procedure QApplication_winFocus(handle: QApplicationH; p1: QWidgetH; p2: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QApplication_winFocus';
procedure QApplication_winMouseButtonUp(); cdecl; external QtShareName name QtNamePrefix + 'QApplication_winMouseButtonUp';
function QApplication_isSessionRestored(handle: QApplicationH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QApplication_isSessionRestored';
procedure QApplication_sessionId(handle: QApplicationH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QApplication_sessionId';
procedure QApplication_sessionKey(handle: QApplicationH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QApplication_sessionKey';
procedure QApplication_commitData(handle: QApplicationH; sm: QSessionManagerH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_commitData';
procedure QApplication_saveState(handle: QApplicationH; sm: QSessionManagerH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_saveState';
procedure QApplication_setInputContext(handle: QApplicationH; p1: QInputContextH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setInputContext';
function QApplication_inputContext(handle: QApplicationH): QInputContextH; cdecl; external QtShareName name QtNamePrefix + 'QApplication_inputContext';
function QApplication_exec(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QApplication_exec';
function QApplication_notify(handle: QApplicationH; p1: QObjectH; p2: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QApplication_notify';
procedure QApplication_setQuitOnLastWindowClosed(quit: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QApplication_setQuitOnLastWindowClosed';
function QApplication_quitOnLastWindowClosed(): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QApplication_quitOnLastWindowClosed';
procedure QApplication_closeAllWindows(); cdecl; external QtShareName name QtNamePrefix + 'QApplication_closeAllWindows';
procedure QApplication_aboutQt(); cdecl; external QtShareName name QtNamePrefix + 'QApplication_aboutQt';


type
  QApplication_lastWindowClosed_Event = procedure () of object cdecl;
  QApplication_focusChanged_Event = procedure (old: QWidgetH; now: QWidgetH) of object cdecl;


type
  QPaletteColorGroup = cardinal; //  QPalette::ColorGroup (4)

const
    QPaletteActive = 0 { $0 };
    QPaletteDisabled = 1 { $1 };
    QPaletteInactive = 2 { $2 };
    QPaletteNColorGroups = 3 { $3 };
    QPaletteCurrent = 4 { $4 };
    QPaletteAll = 5 { $5 };
    QPaletteNormal = 0 { $0 };

type
  QPaletteColorRole = cardinal; //  QPalette::ColorRole (4)

const
    QPaletteWindowText = 0 { $0 };
    QPaletteButton = 1 { $1 };
    QPaletteLight = 2 { $2 };
    QPaletteMidlight = 3 { $3 };
    QPaletteDark = 4 { $4 };
    QPaletteMid = 5 { $5 };
    QPaletteText = 6 { $6 };
    QPaletteBrightText = 7 { $7 };
    QPaletteButtonText = 8 { $8 };
    QPaletteBase = 9 { $9 };
    QPaletteWindow = 10 { $a };
    QPaletteShadow = 11 { $b };
    QPaletteHighlight = 12 { $c };
    QPaletteHighlightedText = 13 { $d };
    QPaletteLink = 14 { $e };
    QPaletteLinkVisited = 15 { $f };
    QPaletteAlternateBase = 16 { $10 };
    QPaletteNColorRoles = 17 { $11 };
    QPaletteNoRole = 17 { $11 };
    QPaletteForeground = 0 { $0 };
    QPaletteBackground = 10 { $a };


function QPalette_create(): QPaletteH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_create';
procedure QPalette_destroy(handle: QPaletteH); cdecl; external QtShareName name QtNamePrefix + 'QPalette_destroy'; 
function QPalette_create(button: PQColor): QPaletteH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_create2';
function QPalette_create(button: QtGlobalColor): QPaletteH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_create3';
function QPalette_create(button: PQColor; window: PQColor): QPaletteH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_create4';
function QPalette_create(windowText: QBrushH; button: QBrushH; light: QBrushH; dark: QBrushH; mid: QBrushH; text: QBrushH; bright_text: QBrushH; base: QBrushH; window: QBrushH): QPaletteH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_create5';
function QPalette_create(windowText: PQColor; window: PQColor; light: PQColor; dark: PQColor; mid: PQColor; text: PQColor; base: PQColor): QPaletteH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_create6';
function QPalette_create(palette: QPaletteH): QPaletteH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_create7';
function QPalette_currentColorGroup(handle: QPaletteH): QPaletteColorGroup; cdecl; external QtShareName name QtNamePrefix + 'QPalette_currentColorGroup';
procedure QPalette_setCurrentColorGroup(handle: QPaletteH; cg: QPaletteColorGroup); cdecl; external QtShareName name QtNamePrefix + 'QPalette_setCurrentColorGroup';
function QPalette_color(handle: QPaletteH; cg: QPaletteColorGroup; cr: QPaletteColorRole): PQColor; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_color';
function QPalette_brush(handle: QPaletteH; cg: QPaletteColorGroup; cr: QPaletteColorRole): QBrushH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_brush';
procedure QPalette_setColor(handle: QPaletteH; cg: QPaletteColorGroup; cr: QPaletteColorRole; color: PQColor); overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_setColor';
procedure QPalette_setColor(handle: QPaletteH; cr: QPaletteColorRole; color: PQColor); overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_setColor2';
procedure QPalette_setBrush(handle: QPaletteH; cr: QPaletteColorRole; brush: QBrushH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_setBrush';
procedure QPalette_setBrush(handle: QPaletteH; cg: QPaletteColorGroup; cr: QPaletteColorRole; brush: QBrushH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_setBrush2';
procedure QPalette_setColorGroup(handle: QPaletteH; cr: QPaletteColorGroup; windowText: QBrushH; button: QBrushH; light: QBrushH; dark: QBrushH; mid: QBrushH; text: QBrushH; bright_text: QBrushH; base: QBrushH; window: QBrushH); cdecl; external QtShareName name QtNamePrefix + 'QPalette_setColorGroup';
function QPalette_isEqual(handle: QPaletteH; cr1: QPaletteColorGroup; cr2: QPaletteColorGroup): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPalette_isEqual';
function QPalette_color(handle: QPaletteH; cr: QPaletteColorRole): PQColor; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_color2';
function QPalette_brush(handle: QPaletteH; cr: QPaletteColorRole): QBrushH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_brush2';
function QPalette_foreground(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_foreground';
function QPalette_windowText(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_windowText';
function QPalette_button(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_button';
function QPalette_light(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_light';
function QPalette_dark(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_dark';
function QPalette_mid(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_mid';
function QPalette_text(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_text';
function QPalette_base(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_base';
function QPalette_alternateBase(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_alternateBase';
function QPalette_background(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_background';
function QPalette_window(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_window';
function QPalette_midlight(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_midlight';
function QPalette_brightText(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_brightText';
function QPalette_buttonText(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_buttonText';
function QPalette_shadow(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_shadow';
function QPalette_highlight(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_highlight';
function QPalette_highlightedText(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_highlightedText';
function QPalette_link(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_link';
function QPalette_linkVisited(handle: QPaletteH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPalette_linkVisited';
function QPalette_serialNumber(handle: QPaletteH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPalette_serialNumber';
procedure QPalette_resolve(handle: QPaletteH; retval: QPaletteH; p1: QPaletteH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_resolve';
function QPalette_resolve(handle: QPaletteH): Cardinal; overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_resolve2';
procedure QPalette_resolve(handle: QPaletteH; mask: Cardinal); overload; cdecl; external QtShareName name QtNamePrefix + 'QPalette_resolve3';


type
  QSizePolicyPolicyFlag = (  //QSizePolicy::PolicyFlag (2s)
    QSizePolicyGrowFlag = 1,
    QSizePolicyExpandFlag = 2,
    QSizePolicyShrinkFlag = 4,
    QSizePolicyIgnoreFlag = 8 );

type
  QSizePolicyPolicy = cardinal; //  QSizePolicy::Policy (4)

const
    QSizePolicyFixed = 0 { $0 };
    QSizePolicyMinimum = 1 { $1 };
    QSizePolicyMaximum = 4 { $4 };
    QSizePolicyPreferred = 5 { $5 };
    QSizePolicyMinimumExpanding = 3 { $3 };
    QSizePolicyExpanding = 7 { $7 };
    QSizePolicyIgnored = 13 { $d };



type
  QKeySequenceSequenceMatch = ( // QKeySequence::SequenceMatch (1)
    QKeySequenceNoMatch, QKeySequencePartialMatch, QKeySequenceExactMatch );

  QKeySequenceSequenceFormat = ( // QKeySequence::SequenceFormat (1)
    QKeySequenceNativeText, QKeySequencePortableText );

function QKeySequence_create(): QKeySequenceH; overload; cdecl; external QtShareName name QtNamePrefix + 'QKeySequence_create';
procedure QKeySequence_destroy(handle: QKeySequenceH); cdecl; external QtShareName name QtNamePrefix + 'QKeySequence_destroy'; 
function QKeySequence_create(key: PWideString): QKeySequenceH; overload; cdecl; external QtShareName name QtNamePrefix + 'QKeySequence_create2';
function QKeySequence_create(k1: Integer; k2: Integer = 0; k3: Integer = 0; k4: Integer = 0): QKeySequenceH; overload; cdecl; external QtShareName name QtNamePrefix + 'QKeySequence_create3';
function QKeySequence_create(ks: QKeySequenceH): QKeySequenceH; overload; cdecl; external QtShareName name QtNamePrefix + 'QKeySequence_create4';
function QKeySequence_count(handle: QKeySequenceH): Cardinal; cdecl; external QtShareName name QtNamePrefix + 'QKeySequence_count';
function QKeySequence_isEmpty(handle: QKeySequenceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QKeySequence_isEmpty';
procedure QKeySequence_toString(handle: QKeySequenceH; retval: PWideString; format: QKeySequenceSequenceFormat = QKeySequencePortableText); cdecl; external QtShareName name QtNamePrefix + 'QKeySequence_toString';
procedure QKeySequence_fromString(retval: QKeySequenceH; str: PWideString; format: QKeySequenceSequenceFormat = QKeySequencePortableText); cdecl; external QtShareName name QtNamePrefix + 'QKeySequence_fromString';
function QKeySequence_matches(handle: QKeySequenceH; seq: QKeySequenceH): QKeySequenceSequenceMatch; cdecl; external QtShareName name QtNamePrefix + 'QKeySequence_matches';
procedure QKeySequence_mnemonic(retval: QKeySequenceH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QKeySequence_mnemonic';


function QWidget_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_create';
procedure QWidget_destroy(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_destroy'; 
function QWidget_devType(handle: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidget_devType';
function QWidget_winId(handle: QWidgetH): Cardinal; cdecl; external QtShareName name QtNamePrefix + 'QWidget_winId';
function QWidget_style(handle: QWidgetH): QStyleH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_style';
procedure QWidget_setStyle(handle: QWidgetH; p1: QStyleH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setStyle';
function QWidget_isTopLevel(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isTopLevel';
function QWidget_isWindow(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isWindow';
function QWidget_isModal(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isModal';
function QWidget_windowModality(handle: QWidgetH): QtWindowModality; cdecl; external QtShareName name QtNamePrefix + 'QWidget_windowModality';
procedure QWidget_setWindowModality(handle: QWidgetH; windowModality: QtWindowModality); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setWindowModality';
function QWidget_isEnabled(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isEnabled';
function QWidget_isEnabledTo(handle: QWidgetH; p1: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isEnabledTo';
function QWidget_isEnabledToTLW(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isEnabledToTLW';
procedure QWidget_setEnabled(handle: QWidgetH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setEnabled';
procedure QWidget_setDisabled(handle: QWidgetH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setDisabled';
procedure QWidget_setWindowModified(handle: QWidgetH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setWindowModified';
procedure QWidget_frameGeometry(handle: QWidgetH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QWidget_frameGeometry';
procedure QWidget_geometry(handle: QWidgetH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QWidget_geometry';
procedure QWidget_normalGeometry(handle: QWidgetH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QWidget_normalGeometry';
function QWidget_x(handle: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidget_x';
function QWidget_y(handle: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidget_y';
procedure QWidget_pos(handle: QWidgetH; retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QWidget_pos';
procedure QWidget_frameSize(handle: QWidgetH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWidget_frameSize';
procedure QWidget_size(handle: QWidgetH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWidget_size';
function QWidget_width(handle: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidget_width';
function QWidget_height(handle: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidget_height';
procedure QWidget_rect(handle: QWidgetH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QWidget_rect';
procedure QWidget_childrenRect(handle: QWidgetH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QWidget_childrenRect';
procedure QWidget_childrenRegion(handle: QWidgetH; retval: QRegionH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_childrenRegion';
procedure QWidget_minimumSize(handle: QWidgetH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWidget_minimumSize';
procedure QWidget_maximumSize(handle: QWidgetH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWidget_maximumSize';
function QWidget_minimumWidth(handle: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidget_minimumWidth';
function QWidget_minimumHeight(handle: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidget_minimumHeight';
function QWidget_maximumWidth(handle: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidget_maximumWidth';
function QWidget_maximumHeight(handle: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidget_maximumHeight';
procedure QWidget_setMinimumSize(handle: QWidgetH; p1: PSize); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setMinimumSize';
procedure QWidget_setMinimumSize(handle: QWidgetH; minw: Integer; minh: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setMinimumSize2';
procedure QWidget_setMaximumSize(handle: QWidgetH; p1: PSize); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setMaximumSize';
procedure QWidget_setMaximumSize(handle: QWidgetH; maxw: Integer; maxh: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setMaximumSize2';
procedure QWidget_setMinimumWidth(handle: QWidgetH; minw: Integer); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setMinimumWidth';
procedure QWidget_setMinimumHeight(handle: QWidgetH; minh: Integer); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setMinimumHeight';
procedure QWidget_setMaximumWidth(handle: QWidgetH; maxw: Integer); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setMaximumWidth';
procedure QWidget_setMaximumHeight(handle: QWidgetH; maxh: Integer); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setMaximumHeight';
procedure QWidget_sizeIncrement(handle: QWidgetH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWidget_sizeIncrement';
procedure QWidget_setSizeIncrement(handle: QWidgetH; p1: PSize); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setSizeIncrement';
procedure QWidget_setSizeIncrement(handle: QWidgetH; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setSizeIncrement2';
procedure QWidget_baseSize(handle: QWidgetH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWidget_baseSize';
procedure QWidget_setBaseSize(handle: QWidgetH; p1: PSize); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setBaseSize';
procedure QWidget_setBaseSize(handle: QWidgetH; basew: Integer; baseh: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setBaseSize2';
procedure QWidget_setFixedSize(handle: QWidgetH; p1: PSize); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setFixedSize';
procedure QWidget_setFixedSize(handle: QWidgetH; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setFixedSize2';
procedure QWidget_setFixedWidth(handle: QWidgetH; w: Integer); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setFixedWidth';
procedure QWidget_setFixedHeight(handle: QWidgetH; h: Integer); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setFixedHeight';
procedure QWidget_mapToGlobal(handle: QWidgetH; retval: PPoint; p1: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QWidget_mapToGlobal';
procedure QWidget_mapFromGlobal(handle: QWidgetH; retval: PPoint; p1: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QWidget_mapFromGlobal';
procedure QWidget_mapToParent(handle: QWidgetH; retval: PPoint; p1: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QWidget_mapToParent';
procedure QWidget_mapFromParent(handle: QWidgetH; retval: PPoint; p1: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QWidget_mapFromParent';
procedure QWidget_mapTo(handle: QWidgetH; retval: PPoint; p1: QWidgetH; p2: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QWidget_mapTo';
procedure QWidget_mapFrom(handle: QWidgetH; retval: PPoint; p1: QWidgetH; p2: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QWidget_mapFrom';
function QWidget_window(handle: QWidgetH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_window';
function QWidget_topLevelWidget(handle: QWidgetH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_topLevelWidget';
function QWidget_palette(handle: QWidgetH): QPaletteH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_palette';
procedure QWidget_setPalette(handle: QWidgetH; p1: QPaletteH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setPalette';
procedure QWidget_setBackgroundRole(handle: QWidgetH; p1: QPaletteColorRole); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setBackgroundRole';
function QWidget_backgroundRole(handle: QWidgetH): QPaletteColorRole; cdecl; external QtShareName name QtNamePrefix + 'QWidget_backgroundRole';
procedure QWidget_setForegroundRole(handle: QWidgetH; p1: QPaletteColorRole); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setForegroundRole';
function QWidget_foregroundRole(handle: QWidgetH): QPaletteColorRole; cdecl; external QtShareName name QtNamePrefix + 'QWidget_foregroundRole';
function QWidget_font(handle: QWidgetH): QFontH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_font';
procedure QWidget_setFont(handle: QWidgetH; p1: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setFont';
procedure QWidget_fontMetrics(handle: QWidgetH; retval: QFontMetricsH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_fontMetrics';
procedure QWidget_fontInfo(handle: QWidgetH; retval: QFontInfoH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_fontInfo';
procedure QWidget_cursor(handle: QWidgetH; retval: QCursorH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_cursor';
procedure QWidget_setCursor(handle: QWidgetH; p1: QCursorH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setCursor';
procedure QWidget_unsetCursor(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_unsetCursor';
procedure QWidget_setMouseTracking(handle: QWidgetH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setMouseTracking';
function QWidget_hasMouseTracking(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_hasMouseTracking';
function QWidget_underMouse(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_underMouse';
procedure QWidget_setMask(handle: QWidgetH; p1: QBitmapH); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setMask';
procedure QWidget_setMask(handle: QWidgetH; p1: QRegionH); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setMask2';
procedure QWidget_mask(handle: QWidgetH; retval: QRegionH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_mask';
procedure QWidget_clearMask(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_clearMask';
procedure QWidget_setWindowTitle(handle: QWidgetH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setWindowTitle';
procedure QWidget_windowTitle(handle: QWidgetH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_windowTitle';
procedure QWidget_setWindowIcon(handle: QWidgetH; icon: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setWindowIcon';
procedure QWidget_windowIcon(handle: QWidgetH; retval: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_windowIcon';
procedure QWidget_setWindowIconText(handle: QWidgetH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setWindowIconText';
procedure QWidget_windowIconText(handle: QWidgetH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_windowIconText';
procedure QWidget_setWindowRole(handle: QWidgetH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setWindowRole';
procedure QWidget_windowRole(handle: QWidgetH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_windowRole';
procedure QWidget_setWindowOpacity(handle: QWidgetH; level: Double); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setWindowOpacity';
function QWidget_windowOpacity(handle: QWidgetH): Double; cdecl; external QtShareName name QtNamePrefix + 'QWidget_windowOpacity';
function QWidget_isWindowModified(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isWindowModified';
procedure QWidget_setToolTip(handle: QWidgetH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setToolTip';
procedure QWidget_toolTip(handle: QWidgetH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_toolTip';
procedure QWidget_setStatusTip(handle: QWidgetH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setStatusTip';
procedure QWidget_statusTip(handle: QWidgetH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_statusTip';
procedure QWidget_setWhatsThis(handle: QWidgetH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setWhatsThis';
procedure QWidget_whatsThis(handle: QWidgetH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_whatsThis';
procedure QWidget_accessibleName(handle: QWidgetH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_accessibleName';
procedure QWidget_setAccessibleName(handle: QWidgetH; name: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setAccessibleName';
procedure QWidget_accessibleDescription(handle: QWidgetH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_accessibleDescription';
procedure QWidget_setAccessibleDescription(handle: QWidgetH; description: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setAccessibleDescription';
procedure QWidget_setLayoutDirection(handle: QWidgetH; direction: QtLayoutDirection); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setLayoutDirection';
function QWidget_layoutDirection(handle: QWidgetH): QtLayoutDirection; cdecl; external QtShareName name QtNamePrefix + 'QWidget_layoutDirection';
procedure QWidget_unsetLayoutDirection(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_unsetLayoutDirection';
function QWidget_isRightToLeft(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isRightToLeft';
function QWidget_isLeftToRight(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isLeftToRight';
procedure QWidget_setFocus(handle: QWidgetH); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setFocus';
function QWidget_isActiveWindow(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isActiveWindow';
procedure QWidget_activateWindow(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_activateWindow';
procedure QWidget_clearFocus(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_clearFocus';
procedure QWidget_setFocus(handle: QWidgetH; reason: QtFocusReason); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setFocus2';
function QWidget_focusPolicy(handle: QWidgetH): QtFocusPolicy; cdecl; external QtShareName name QtNamePrefix + 'QWidget_focusPolicy';
procedure QWidget_setFocusPolicy(handle: QWidgetH; policy: QtFocusPolicy); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setFocusPolicy';
function QWidget_hasFocus(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_hasFocus';
procedure QWidget_setTabOrder(p1: QWidgetH; p2: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setTabOrder';
procedure QWidget_setFocusProxy(handle: QWidgetH; p1: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setFocusProxy';
function QWidget_focusProxy(handle: QWidgetH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_focusProxy';
function QWidget_contextMenuPolicy(handle: QWidgetH): QtContextMenuPolicy; cdecl; external QtShareName name QtNamePrefix + 'QWidget_contextMenuPolicy';
procedure QWidget_setContextMenuPolicy(handle: QWidgetH; policy: QtContextMenuPolicy); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setContextMenuPolicy';
procedure QWidget_grabMouse(handle: QWidgetH); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_grabMouse';
procedure QWidget_grabMouse(handle: QWidgetH; p1: QCursorH); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_grabMouse2';
procedure QWidget_releaseMouse(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_releaseMouse';
procedure QWidget_grabKeyboard(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_grabKeyboard';
procedure QWidget_releaseKeyboard(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_releaseKeyboard';
function QWidget_grabShortcut(handle: QWidgetH; key: QKeySequenceH; context: QtShortcutContext = QtWindowShortcut): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidget_grabShortcut';
procedure QWidget_releaseShortcut(handle: QWidgetH; id: Integer); cdecl; external QtShareName name QtNamePrefix + 'QWidget_releaseShortcut';
procedure QWidget_setShortcutEnabled(handle: QWidgetH; id: Integer; enable: Boolean = True); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setShortcutEnabled';
function QWidget_mouseGrabber(): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_mouseGrabber';
function QWidget_keyboardGrabber(): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_keyboardGrabber';
function QWidget_updatesEnabled(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_updatesEnabled';
procedure QWidget_setUpdatesEnabled(handle: QWidgetH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setUpdatesEnabled';
procedure QWidget_update(handle: QWidgetH); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_update';
procedure QWidget_repaint(handle: QWidgetH); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_repaint';
procedure QWidget_update(handle: QWidgetH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_update2';
procedure QWidget_update(handle: QWidgetH; p1: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_update3';
procedure QWidget_update(handle: QWidgetH; p1: QRegionH); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_update4';
procedure QWidget_repaint(handle: QWidgetH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_repaint2';
procedure QWidget_repaint(handle: QWidgetH; p1: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_repaint3';
procedure QWidget_repaint(handle: QWidgetH; p1: QRegionH); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_repaint4';
procedure QWidget_setVisible(handle: QWidgetH; visible: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setVisible';
procedure QWidget_setHidden(handle: QWidgetH; hidden: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setHidden';
procedure QWidget_show(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_show';
procedure QWidget_hide(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_hide';
procedure QWidget_setShown(handle: QWidgetH; shown: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setShown';
procedure QWidget_showMinimized(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_showMinimized';
procedure QWidget_showMaximized(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_showMaximized';
procedure QWidget_showFullScreen(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_showFullScreen';
procedure QWidget_showNormal(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_showNormal';
function QWidget_close(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_close';
procedure QWidget_raise(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_raise';
procedure QWidget_lower(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_lower';
procedure QWidget_stackUnder(handle: QWidgetH; p1: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_stackUnder';
procedure QWidget_move(handle: QWidgetH; x: Integer; y: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_move';
procedure QWidget_move(handle: QWidgetH; p1: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_move2';
procedure QWidget_resize(handle: QWidgetH; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_resize';
procedure QWidget_resize(handle: QWidgetH; p1: PSize); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_resize2';
procedure QWidget_setGeometry(handle: QWidgetH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setGeometry';
procedure QWidget_setGeometry(handle: QWidgetH; p1: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setGeometry2';
procedure QWidget_adjustSize(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_adjustSize';
function QWidget_isVisible(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isVisible';
function QWidget_isVisibleTo(handle: QWidgetH; p1: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isVisibleTo';
function QWidget_isHidden(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isHidden';
function QWidget_isMinimized(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isMinimized';
function QWidget_isMaximized(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isMaximized';
function QWidget_isFullScreen(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isFullScreen';
function QWidget_windowState(handle: QWidgetH): QtWindowStates; cdecl; external QtShareName name QtNamePrefix + 'QWidget_windowState';
procedure QWidget_setWindowState(handle: QWidgetH; state: QtWindowStates); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setWindowState';
procedure QWidget_overrideWindowState(handle: QWidgetH; state: QtWindowStates); cdecl; external QtShareName name QtNamePrefix + 'QWidget_overrideWindowState';
procedure QWidget_sizeHint(handle: QWidgetH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWidget_sizeHint';
procedure QWidget_minimumSizeHint(handle: QWidgetH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWidget_minimumSizeHint';
procedure QWidget_sizePolicy(handle: QWidgetH; retval: PSizePolicy); cdecl; external QtShareName name QtNamePrefix + 'QWidget_sizePolicy';
procedure QWidget_setSizePolicy(handle: QWidgetH; p1: PSizePolicy); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setSizePolicy';
procedure QWidget_setSizePolicy(handle: QWidgetH; horizontal: QSizePolicyPolicy; vertical: QSizePolicyPolicy); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setSizePolicy2';
function QWidget_heightForWidth(handle: QWidgetH; p1: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidget_heightForWidth';
procedure QWidget_visibleRegion(handle: QWidgetH; retval: QRegionH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_visibleRegion';
procedure QWidget_setContentsMargins(handle: QWidgetH; left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setContentsMargins';
procedure QWidget_getContentsMargins(handle: QWidgetH; left: PInteger; top: PInteger; right: PInteger; bottom: PInteger); cdecl; external QtShareName name QtNamePrefix + 'QWidget_getContentsMargins';
procedure QWidget_contentsRect(handle: QWidgetH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QWidget_contentsRect';
function QWidget_layout(handle: QWidgetH): QLayoutH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_layout';
procedure QWidget_setLayout(handle: QWidgetH; p1: QLayoutH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setLayout';
procedure QWidget_updateGeometry(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_updateGeometry';
procedure QWidget_setParent(handle: QWidgetH; parent: QWidgetH); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setParent';
procedure QWidget_setParent(handle: QWidgetH; parent: QWidgetH; f: QtWindowFlags); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_setParent2';
procedure QWidget_scroll(handle: QWidgetH; dx: Integer; dy: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_scroll';
procedure QWidget_scroll(handle: QWidgetH; dx: Integer; dy: Integer; p3: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_scroll2';
function QWidget_focusWidget(handle: QWidgetH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_focusWidget';
function QWidget_nextInFocusChain(handle: QWidgetH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_nextInFocusChain';
function QWidget_acceptDrops(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_acceptDrops';
procedure QWidget_setAcceptDrops(handle: QWidgetH; _on: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setAcceptDrops';
procedure QWidget_addAction(handle: QWidgetH; action: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_addAction';
procedure QWidget_addActions(handle: QWidgetH; actions: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QWidget_addActions';
procedure QWidget_insertAction(handle: QWidgetH; before: QActionH; action: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_insertAction';
procedure QWidget_insertActions(handle: QWidgetH; before: QActionH; actions: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QWidget_insertActions';
procedure QWidget_removeAction(handle: QWidgetH; action: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_removeAction';
procedure QWidget_actions(handle: QWidgetH; retval: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QWidget_actions';
function QWidget_parentWidget(handle: QWidgetH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_parentWidget';
procedure QWidget_setWindowFlags(handle: QWidgetH; _type: QtWindowFlags); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setWindowFlags';
function QWidget_windowFlags(handle: QWidgetH): QtWindowFlags; cdecl; external QtShareName name QtNamePrefix + 'QWidget_windowFlags';
procedure QWidget_overrideWindowFlags(handle: QWidgetH; _type: QtWindowFlags); cdecl; external QtShareName name QtNamePrefix + 'QWidget_overrideWindowFlags';
function QWidget_windowType(handle: QWidgetH): QtWindowType; cdecl; external QtShareName name QtNamePrefix + 'QWidget_windowType';
function QWidget_find(p1: Cardinal): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_find';
function QWidget_childAt(handle: QWidgetH; x: Integer; y: Integer): QWidgetH; overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_childAt';
function QWidget_childAt(handle: QWidgetH; p: PPoint): QWidgetH; overload; cdecl; external QtShareName name QtNamePrefix + 'QWidget_childAt2';
function QWidget_getDC(handle: QWidgetH): HDC; cdecl; external QtShareName name QtNamePrefix + 'QWidget_getDC';
procedure QWidget_releaseDC(handle: QWidgetH; p1: HDC); cdecl; external QtShareName name QtNamePrefix + 'QWidget_releaseDC';
procedure QWidget_setAttribute(handle: QWidgetH; p1: QtWidgetAttribute; _on: Boolean = True); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setAttribute';
function QWidget_testAttribute(handle: QWidgetH; p1: QtWidgetAttribute): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_testAttribute';
function QWidget_paintEngine(handle: QWidgetH): QPaintEngineH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_paintEngine';
procedure QWidget_ensurePolished(handle: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_ensurePolished';
function QWidget_inputContext(handle: QWidgetH): QInputContextH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_inputContext';
procedure QWidget_setInputContext(handle: QWidgetH; p1: QInputContextH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setInputContext';
function QWidget_isAncestorOf(handle: QWidgetH; child: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_isAncestorOf';
function QWidget_autoFillBackground(handle: QWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidget_autoFillBackground';
procedure QWidget_setAutoFillBackground(handle: QWidgetH; enabled: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QWidget_setAutoFillBackground';
procedure QWidget_inputMethodQuery(handle: QWidgetH; retval: QVariantH; p1: QtInputMethodQuery); cdecl; external QtShareName name QtNamePrefix + 'QWidget_inputMethodQuery';
function QWidget_to_QPaintDevice(handle: QWidgetH): QPaintDeviceH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_to_QPaintDevice';

type
  QWidget_customContextMenuRequested_Event = procedure (pos: PPoint) of object cdecl;


procedure QLayoutItem_sizeHint(handle: QLayoutItemH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_sizeHint';
procedure QLayoutItem_minimumSize(handle: QLayoutItemH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_minimumSize';
procedure QLayoutItem_maximumSize(handle: QLayoutItemH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_maximumSize';
function QLayoutItem_expandingDirections(handle: QLayoutItemH): QtOrientations; cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_expandingDirections';
procedure QLayoutItem_setGeometry(handle: QLayoutItemH; p1: PRect); cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_setGeometry';
procedure QLayoutItem_geometry(handle: QLayoutItemH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_geometry';
function QLayoutItem_isEmpty(handle: QLayoutItemH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_isEmpty';
function QLayoutItem_hasHeightForWidth(handle: QLayoutItemH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_hasHeightForWidth';
function QLayoutItem_heightForWidth(handle: QLayoutItemH; p1: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_heightForWidth';
function QLayoutItem_minimumHeightForWidth(handle: QLayoutItemH; p1: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_minimumHeightForWidth';
procedure QLayoutItem_invalidate(handle: QLayoutItemH); cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_invalidate';
function QLayoutItem_widget(handle: QLayoutItemH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_widget';
function QLayoutItem_layout(handle: QLayoutItemH): QLayoutH; cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_layout';
function QLayoutItem_spacerItem(handle: QLayoutItemH): QSpacerItemH; cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_spacerItem';
function QLayoutItem_alignment(handle: QLayoutItemH): QtAlignment; cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_alignment';
procedure QLayoutItem_setAlignment(handle: QLayoutItemH; a: QtAlignment); cdecl; external QtShareName name QtNamePrefix + 'QLayoutItem_setAlignment';

function QSpacerItem_create(w: Integer; h: Integer; hData: QSizePolicyPolicy = QSizePolicyMinimum; vData: QSizePolicyPolicy = QSizePolicyMinimum): QSpacerItemH; cdecl; external QtShareName name QtNamePrefix + 'QSpacerItem_create';
procedure QSpacerItem_destroy(handle: QSpacerItemH); cdecl; external QtShareName name QtNamePrefix + 'QSpacerItem_destroy'; 
procedure QSpacerItem_changeSize(handle: QSpacerItemH; w: Integer; h: Integer; hData: QSizePolicyPolicy = QSizePolicyMinimum; vData: QSizePolicyPolicy = QSizePolicyMinimum); cdecl; external QtShareName name QtNamePrefix + 'QSpacerItem_changeSize';
procedure QSpacerItem_sizeHint(handle: QSpacerItemH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QSpacerItem_sizeHint';
procedure QSpacerItem_minimumSize(handle: QSpacerItemH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QSpacerItem_minimumSize';
procedure QSpacerItem_maximumSize(handle: QSpacerItemH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QSpacerItem_maximumSize';
function QSpacerItem_expandingDirections(handle: QSpacerItemH): QtOrientations; cdecl; external QtShareName name QtNamePrefix + 'QSpacerItem_expandingDirections';
function QSpacerItem_isEmpty(handle: QSpacerItemH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QSpacerItem_isEmpty';
procedure QSpacerItem_setGeometry(handle: QSpacerItemH; p1: PRect); cdecl; external QtShareName name QtNamePrefix + 'QSpacerItem_setGeometry';
procedure QSpacerItem_geometry(handle: QSpacerItemH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QSpacerItem_geometry';
function QSpacerItem_spacerItem(handle: QSpacerItemH): QSpacerItemH; cdecl; external QtShareName name QtNamePrefix + 'QSpacerItem_spacerItem';

function QWidgetItem_create(w: QWidgetH): QWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_create';
procedure QWidgetItem_destroy(handle: QWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_destroy'; 
procedure QWidgetItem_sizeHint(handle: QWidgetItemH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_sizeHint';
procedure QWidgetItem_minimumSize(handle: QWidgetItemH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_minimumSize';
procedure QWidgetItem_maximumSize(handle: QWidgetItemH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_maximumSize';
function QWidgetItem_expandingDirections(handle: QWidgetItemH): QtOrientations; cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_expandingDirections';
function QWidgetItem_isEmpty(handle: QWidgetItemH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_isEmpty';
procedure QWidgetItem_setGeometry(handle: QWidgetItemH; p1: PRect); cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_setGeometry';
procedure QWidgetItem_geometry(handle: QWidgetItemH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_geometry';
function QWidgetItem_widget(handle: QWidgetItemH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_widget';
function QWidgetItem_hasHeightForWidth(handle: QWidgetItemH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_hasHeightForWidth';
function QWidgetItem_heightForWidth(handle: QWidgetItemH; p1: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWidgetItem_heightForWidth';


type
  QLayoutSizeConstraint = ( // QLayout::SizeConstraint (1)
    QLayoutSetDefaultConstraint, QLayoutSetNoConstraint, QLayoutSetMinimumSize, QLayoutSetFixedSize, QLayoutSetMaximumSize, QLayoutSetMinAndMaxSize );

function QLayout_margin(handle: QLayoutH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLayout_margin';
function QLayout_spacing(handle: QLayoutH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLayout_spacing';
procedure QLayout_setMargin(handle: QLayoutH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QLayout_setMargin';
procedure QLayout_setSpacing(handle: QLayoutH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QLayout_setSpacing';
function QLayout_setAlignment(handle: QLayoutH; w: QWidgetH; alignment: QtAlignment): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QLayout_setAlignment';
function QLayout_setAlignment(handle: QLayoutH; l: QLayoutH; alignment: QtAlignment): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QLayout_setAlignment2';
procedure QLayout_setSizeConstraint(handle: QLayoutH; p1: QLayoutSizeConstraint); cdecl; external QtShareName name QtNamePrefix + 'QLayout_setSizeConstraint';
function QLayout_sizeConstraint(handle: QLayoutH): QLayoutSizeConstraint; cdecl; external QtShareName name QtNamePrefix + 'QLayout_sizeConstraint';
procedure QLayout_setMenuBar(handle: QLayoutH; w: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QLayout_setMenuBar';
function QLayout_menuBar(handle: QLayoutH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QLayout_menuBar';
function QLayout_parentWidget(handle: QLayoutH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QLayout_parentWidget';
procedure QLayout_invalidate(handle: QLayoutH); cdecl; external QtShareName name QtNamePrefix + 'QLayout_invalidate';
procedure QLayout_geometry(handle: QLayoutH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QLayout_geometry';
function QLayout_activate(handle: QLayoutH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLayout_activate';
procedure QLayout_update(handle: QLayoutH); cdecl; external QtShareName name QtNamePrefix + 'QLayout_update';
procedure QLayout_addWidget(handle: QLayoutH; w: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QLayout_addWidget';
procedure QLayout_addItem(handle: QLayoutH; p1: QLayoutItemH); cdecl; external QtShareName name QtNamePrefix + 'QLayout_addItem';
procedure QLayout_removeWidget(handle: QLayoutH; w: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QLayout_removeWidget';
procedure QLayout_removeItem(handle: QLayoutH; p1: QLayoutItemH); cdecl; external QtShareName name QtNamePrefix + 'QLayout_removeItem';
function QLayout_expandingDirections(handle: QLayoutH): QtOrientations; cdecl; external QtShareName name QtNamePrefix + 'QLayout_expandingDirections';
procedure QLayout_minimumSize(handle: QLayoutH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLayout_minimumSize';
procedure QLayout_maximumSize(handle: QLayoutH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLayout_maximumSize';
procedure QLayout_setGeometry(handle: QLayoutH; p1: PRect); cdecl; external QtShareName name QtNamePrefix + 'QLayout_setGeometry';
function QLayout_itemAt(handle: QLayoutH; index: Integer): QLayoutItemH; cdecl; external QtShareName name QtNamePrefix + 'QLayout_itemAt';
function QLayout_takeAt(handle: QLayoutH; index: Integer): QLayoutItemH; cdecl; external QtShareName name QtNamePrefix + 'QLayout_takeAt';
function QLayout_indexOf(handle: QLayoutH; p1: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLayout_indexOf';
function QLayout_count(handle: QLayoutH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLayout_count';
function QLayout_isEmpty(handle: QLayoutH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLayout_isEmpty';
function QLayout_totalHeightForWidth(handle: QLayoutH; w: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLayout_totalHeightForWidth';
procedure QLayout_totalMinimumSize(handle: QLayoutH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLayout_totalMinimumSize';
procedure QLayout_totalMaximumSize(handle: QLayoutH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLayout_totalMaximumSize';
procedure QLayout_totalSizeHint(handle: QLayoutH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLayout_totalSizeHint';
function QLayout_layout(handle: QLayoutH): QLayoutH; cdecl; external QtShareName name QtNamePrefix + 'QLayout_layout';
procedure QLayout_setEnabled(handle: QLayoutH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLayout_setEnabled';
function QLayout_isEnabled(handle: QLayoutH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLayout_isEnabled';
procedure QLayout_closestAcceptableSize(retval: PSize; w: QWidgetH; s: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLayout_closestAcceptableSize';
function QLayout_to_QLayoutItem(handle: QLayoutH): QLayoutItemH; cdecl; external QtShareName name QtNamePrefix + 'QLayout_to_QLayoutItem';
type
  QBoxLayoutDirection = cardinal; //  QBoxLayout::Direction (4)

const
    QBoxLayoutLeftToRight = 0 { $0 };
    QBoxLayoutRightToLeft = 1 { $1 };
    QBoxLayoutTopToBottom = 2 { $2 };
    QBoxLayoutBottomToTop = 3 { $3 };
    QBoxLayoutDown = 2 { $2 };
    QBoxLayoutUp = 3 { $3 };


function QBoxLayout_create(p1: QBoxLayoutDirection; parent: QWidgetH = nil): QBoxLayoutH; cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_create';
procedure QBoxLayout_destroy(handle: QBoxLayoutH); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_destroy'; 
function QBoxLayout_direction(handle: QBoxLayoutH): QBoxLayoutDirection; cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_direction';
procedure QBoxLayout_setDirection(handle: QBoxLayoutH; p1: QBoxLayoutDirection); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_setDirection';
procedure QBoxLayout_addSpacing(handle: QBoxLayoutH; size: Integer); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_addSpacing';
procedure QBoxLayout_addStretch(handle: QBoxLayoutH; stretch: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_addStretch';
procedure QBoxLayout_addWidget(handle: QBoxLayoutH; p1: QWidgetH; stretch: Integer = 0; alignment: QtAlignment = 0); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_addWidget';
procedure QBoxLayout_addLayout(handle: QBoxLayoutH; layout: QLayoutH; stretch: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_addLayout';
procedure QBoxLayout_addStrut(handle: QBoxLayoutH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_addStrut';
procedure QBoxLayout_addItem(handle: QBoxLayoutH; p1: QLayoutItemH); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_addItem';
procedure QBoxLayout_insertSpacing(handle: QBoxLayoutH; index: Integer; size: Integer); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_insertSpacing';
procedure QBoxLayout_insertStretch(handle: QBoxLayoutH; index: Integer; stretch: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_insertStretch';
procedure QBoxLayout_insertWidget(handle: QBoxLayoutH; index: Integer; widget: QWidgetH; stretch: Integer = 0; alignment: QtAlignment = 0); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_insertWidget';
procedure QBoxLayout_insertLayout(handle: QBoxLayoutH; index: Integer; layout: QLayoutH; stretch: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_insertLayout';
function QBoxLayout_setStretchFactor(handle: QBoxLayoutH; w: QWidgetH; stretch: Integer): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_setStretchFactor';
function QBoxLayout_setStretchFactor(handle: QBoxLayoutH; l: QLayoutH; stretch: Integer): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_setStretchFactor2';
procedure QBoxLayout_sizeHint(handle: QBoxLayoutH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_sizeHint';
procedure QBoxLayout_minimumSize(handle: QBoxLayoutH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_minimumSize';
procedure QBoxLayout_maximumSize(handle: QBoxLayoutH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_maximumSize';
function QBoxLayout_hasHeightForWidth(handle: QBoxLayoutH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_hasHeightForWidth';
function QBoxLayout_heightForWidth(handle: QBoxLayoutH; p1: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_heightForWidth';
function QBoxLayout_minimumHeightForWidth(handle: QBoxLayoutH; p1: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_minimumHeightForWidth';
function QBoxLayout_expandingDirections(handle: QBoxLayoutH): QtOrientations; cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_expandingDirections';
procedure QBoxLayout_invalidate(handle: QBoxLayoutH); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_invalidate';
function QBoxLayout_itemAt(handle: QBoxLayoutH; p1: Integer): QLayoutItemH; cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_itemAt';
function QBoxLayout_takeAt(handle: QBoxLayoutH; p1: Integer): QLayoutItemH; cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_takeAt';
function QBoxLayout_count(handle: QBoxLayoutH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_count';
procedure QBoxLayout_setGeometry(handle: QBoxLayoutH; p1: PRect); cdecl; external QtShareName name QtNamePrefix + 'QBoxLayout_setGeometry';

function QHBoxLayout_create(): QHBoxLayoutH; overload; cdecl; external QtShareName name QtNamePrefix + 'QHBoxLayout_create';
procedure QHBoxLayout_destroy(handle: QHBoxLayoutH); cdecl; external QtShareName name QtNamePrefix + 'QHBoxLayout_destroy'; 
function QHBoxLayout_create(parent: QWidgetH): QHBoxLayoutH; overload; cdecl; external QtShareName name QtNamePrefix + 'QHBoxLayout_create2';

function QVBoxLayout_create(): QVBoxLayoutH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVBoxLayout_create';
procedure QVBoxLayout_destroy(handle: QVBoxLayoutH); cdecl; external QtShareName name QtNamePrefix + 'QVBoxLayout_destroy'; 
function QVBoxLayout_create(parent: QWidgetH): QVBoxLayoutH; overload; cdecl; external QtShareName name QtNamePrefix + 'QVBoxLayout_create2';


type
  QActionActionEvent = ( // QAction::ActionEvent (1)
    QActionTrigger, QActionHover );

function QAction_create(parent: QObjectH): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QAction_create';
procedure QAction_destroy(handle: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QAction_destroy'; 
function QAction_create(text: PWideString; parent: QObjectH): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QAction_create2';
function QAction_create(icon: QIconH; text: PWideString; parent: QObjectH): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QAction_create3';
procedure QAction_setActionGroup(handle: QActionH; group: QActionGroupH); cdecl; external QtShareName name QtNamePrefix + 'QAction_setActionGroup';
function QAction_actionGroup(handle: QActionH): QActionGroupH; cdecl; external QtShareName name QtNamePrefix + 'QAction_actionGroup';
procedure QAction_setIcon(handle: QActionH; icon: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QAction_setIcon';
procedure QAction_icon(handle: QActionH; retval: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QAction_icon';
procedure QAction_setText(handle: QActionH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAction_setText';
procedure QAction_text(handle: QActionH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAction_text';
procedure QAction_setIconText(handle: QActionH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAction_setIconText';
procedure QAction_iconText(handle: QActionH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAction_iconText';
procedure QAction_setToolTip(handle: QActionH; tip: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAction_setToolTip';
procedure QAction_toolTip(handle: QActionH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAction_toolTip';
procedure QAction_setStatusTip(handle: QActionH; statusTip: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAction_setStatusTip';
procedure QAction_statusTip(handle: QActionH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAction_statusTip';
procedure QAction_setWhatsThis(handle: QActionH; what: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAction_setWhatsThis';
procedure QAction_whatsThis(handle: QActionH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAction_whatsThis';
function QAction_menu(handle: QActionH): QMenuH; cdecl; external QtShareName name QtNamePrefix + 'QAction_menu';
procedure QAction_setMenu(handle: QActionH; menu: QMenuH); cdecl; external QtShareName name QtNamePrefix + 'QAction_setMenu';
procedure QAction_setSeparator(handle: QActionH; b: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAction_setSeparator';
function QAction_isSeparator(handle: QActionH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAction_isSeparator';
procedure QAction_setShortcut(handle: QActionH; shortcut: QKeySequenceH); cdecl; external QtShareName name QtNamePrefix + 'QAction_setShortcut';
procedure QAction_shortcut(handle: QActionH; retval: QKeySequenceH); cdecl; external QtShareName name QtNamePrefix + 'QAction_shortcut';
procedure QAction_setShortcutContext(handle: QActionH; context: QtShortcutContext); cdecl; external QtShareName name QtNamePrefix + 'QAction_setShortcutContext';
function QAction_shortcutContext(handle: QActionH): QtShortcutContext; cdecl; external QtShareName name QtNamePrefix + 'QAction_shortcutContext';
procedure QAction_setFont(handle: QActionH; font: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QAction_setFont';
procedure QAction_font(handle: QActionH; retval: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QAction_font';
procedure QAction_setCheckable(handle: QActionH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAction_setCheckable';
function QAction_isCheckable(handle: QActionH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAction_isCheckable';
procedure QAction_data(handle: QActionH; retval: QVariantH); cdecl; external QtShareName name QtNamePrefix + 'QAction_data';
procedure QAction_setData(handle: QActionH; _var: QVariantH); cdecl; external QtShareName name QtNamePrefix + 'QAction_setData';
function QAction_isChecked(handle: QActionH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAction_isChecked';
function QAction_isEnabled(handle: QActionH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAction_isEnabled';
function QAction_isVisible(handle: QActionH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAction_isVisible';
procedure QAction_activate(handle: QActionH; event: QActionActionEvent); cdecl; external QtShareName name QtNamePrefix + 'QAction_activate';
function QAction_showStatusText(handle: QActionH; widget: QWidgetH = nil): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAction_showStatusText';
function QAction_parentWidget(handle: QActionH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QAction_parentWidget';
procedure QAction_trigger(handle: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QAction_trigger';
procedure QAction_hover(handle: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QAction_hover';
procedure QAction_setChecked(handle: QActionH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAction_setChecked';
procedure QAction_toggle(handle: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QAction_toggle';
procedure QAction_setEnabled(handle: QActionH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAction_setEnabled';
procedure QAction_setDisabled(handle: QActionH; b: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAction_setDisabled';
procedure QAction_setVisible(handle: QActionH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAction_setVisible';


type
  QAction_changed_Event = procedure () of object cdecl;
  QAction_triggered_Event = procedure (checked: Boolean = False) of object cdecl;
  QAction_triggered2_Event = procedure () of object cdecl;
  QAction_hovered_Event = procedure () of object cdecl;
  QAction_toggled_Event = procedure (p1: Boolean) of object cdecl;



type
  QTabletEventTabletDevice = ( // QTabletEvent::TabletDevice (1)
    QTabletEventNoDevice, QTabletEventPuck, QTabletEventStylus, QTabletEventAirbrush, QTabletEventFourDMouse, QTabletEventXFreeEraser, QTabletEventRotationStylus );

  QTabletEventPointerType = ( // QTabletEvent::PointerType (1)
    QTabletEventUnknownPointer, QTabletEventPen, QTabletEventCursor, QTabletEventEraser );


type
  QContextMenuEventReason = ( // QContextMenuEvent::Reason (1)
    QContextMenuEventMouse, QContextMenuEventKeyboard, QContextMenuEventOther );


type
  QInputMethodEventAttributeType = ( // QInputMethodEvent::AttributeType (1)
    QInputMethodEventTextFormat, QInputMethodEventCursor, QInputMethodEventLanguage, QInputMethodEventRuby );

function QInputEvent_create(_type: QEventType; modifiers: QtKeyboardModifiers = QtNoModifier): QInputEventH; cdecl; external QtShareName name QtNamePrefix + 'QInputEvent_create';
procedure QInputEvent_destroy(handle: QInputEventH); cdecl; external QtShareName name QtNamePrefix + 'QInputEvent_destroy'; 
function QInputEvent_modifiers(handle: QInputEventH): QtKeyboardModifiers; cdecl; external QtShareName name QtNamePrefix + 'QInputEvent_modifiers';

function QMouseEvent_create(_type: QEventType; pos: PPoint; button: QtMouseButton; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers): QMouseEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMouseEvent_create';
procedure QMouseEvent_destroy(handle: QMouseEventH); cdecl; external QtShareName name QtNamePrefix + 'QMouseEvent_destroy'; 
function QMouseEvent_create(_type: QEventType; pos: PPoint; globalPos: PPoint; button: QtMouseButton; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers): QMouseEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMouseEvent_create2';
function QMouseEvent_pos(handle: QMouseEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QMouseEvent_pos';
function QMouseEvent_globalPos(handle: QMouseEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QMouseEvent_globalPos';
function QMouseEvent_x(handle: QMouseEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QMouseEvent_x';
function QMouseEvent_y(handle: QMouseEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QMouseEvent_y';
function QMouseEvent_globalX(handle: QMouseEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QMouseEvent_globalX';
function QMouseEvent_globalY(handle: QMouseEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QMouseEvent_globalY';
function QMouseEvent_button(handle: QMouseEventH): QtMouseButton; cdecl; external QtShareName name QtNamePrefix + 'QMouseEvent_button';
function QMouseEvent_buttons(handle: QMouseEventH): QtMouseButtons; cdecl; external QtShareName name QtNamePrefix + 'QMouseEvent_buttons';

function QHoverEvent_create(_type: QEventType; pos: PPoint; oldPos: PPoint): QHoverEventH; cdecl; external QtShareName name QtNamePrefix + 'QHoverEvent_create';
procedure QHoverEvent_destroy(handle: QHoverEventH); cdecl; external QtShareName name QtNamePrefix + 'QHoverEvent_destroy'; 
function QHoverEvent_pos(handle: QHoverEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QHoverEvent_pos';
function QHoverEvent_oldPos(handle: QHoverEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QHoverEvent_oldPos';

function QWheelEvent_create(pos: PPoint; delta: Integer; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers; orient: QtOrientation = QtVertical): QWheelEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_create';
procedure QWheelEvent_destroy(handle: QWheelEventH); cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_destroy'; 
function QWheelEvent_create(pos: PPoint; globalPos: PPoint; delta: Integer; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers; orient: QtOrientation = QtVertical): QWheelEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_create2';
function QWheelEvent_delta(handle: QWheelEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_delta';
function QWheelEvent_pos(handle: QWheelEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_pos';
function QWheelEvent_globalPos(handle: QWheelEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_globalPos';
function QWheelEvent_x(handle: QWheelEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_x';
function QWheelEvent_y(handle: QWheelEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_y';
function QWheelEvent_globalX(handle: QWheelEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_globalX';
function QWheelEvent_globalY(handle: QWheelEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_globalY';
function QWheelEvent_buttons(handle: QWheelEventH): QtMouseButtons; cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_buttons';
function QWheelEvent_orientation(handle: QWheelEventH): QtOrientation; cdecl; external QtShareName name QtNamePrefix + 'QWheelEvent_orientation';

function QTabletEvent_create(t: QEventType; pos: PPoint; globalPos: PPoint; hiResGlobalPos: QPointFH; device: Integer; pointerType: Integer; pressure: Double; xTilt: Integer; yTilt: Integer; tangentialPressure: Double; rotation: Double; z: Integer; keyState: QtKeyboardModifiers; uniqueID: int64): QTabletEventH; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_create';
procedure QTabletEvent_destroy(handle: QTabletEventH); cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_destroy'; 
function QTabletEvent_pos(handle: QTabletEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_pos';
function QTabletEvent_globalPos(handle: QTabletEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_globalPos';
function QTabletEvent_hiResGlobalPos(handle: QTabletEventH): QPointFH; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_hiResGlobalPos';
function QTabletEvent_x(handle: QTabletEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_x';
function QTabletEvent_y(handle: QTabletEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_y';
function QTabletEvent_globalX(handle: QTabletEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_globalX';
function QTabletEvent_globalY(handle: QTabletEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_globalY';
function QTabletEvent_hiResGlobalX(handle: QTabletEventH): Double; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_hiResGlobalX';
function QTabletEvent_hiResGlobalY(handle: QTabletEventH): Double; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_hiResGlobalY';
function QTabletEvent_device(handle: QTabletEventH): QTabletEventTabletDevice; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_device';
function QTabletEvent_pointerType(handle: QTabletEventH): QTabletEventPointerType; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_pointerType';
function QTabletEvent_uniqueId(handle: QTabletEventH): int64; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_uniqueId';
function QTabletEvent_pressure(handle: QTabletEventH): Double; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_pressure';
function QTabletEvent_z(handle: QTabletEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_z';
function QTabletEvent_tangentialPressure(handle: QTabletEventH): Double; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_tangentialPressure';
function QTabletEvent_rotation(handle: QTabletEventH): Double; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_rotation';
function QTabletEvent_xTilt(handle: QTabletEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_xTilt';
function QTabletEvent_yTilt(handle: QTabletEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabletEvent_yTilt';

function QKeyEvent_create(_type: QEventType; key: Integer; modifiers: QtKeyboardModifiers; text: PWideString = nil; autorep: Boolean = False; count: Word = 1): QKeyEventH; cdecl; external QtShareName name QtNamePrefix + 'QKeyEvent_create';
procedure QKeyEvent_destroy(handle: QKeyEventH); cdecl; external QtShareName name QtNamePrefix + 'QKeyEvent_destroy'; 
function QKeyEvent_key(handle: QKeyEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QKeyEvent_key';
function QKeyEvent_modifiers(handle: QKeyEventH): QtKeyboardModifiers; cdecl; external QtShareName name QtNamePrefix + 'QKeyEvent_modifiers';
procedure QKeyEvent_text(handle: QKeyEventH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QKeyEvent_text';
function QKeyEvent_isAutoRepeat(handle: QKeyEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QKeyEvent_isAutoRepeat';
function QKeyEvent_count(handle: QKeyEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QKeyEvent_count';

function QFocusEvent_create(_type: QEventType; reason: QtFocusReason = QtOtherFocusReason): QFocusEventH; cdecl; external QtShareName name QtNamePrefix + 'QFocusEvent_create';
procedure QFocusEvent_destroy(handle: QFocusEventH); cdecl; external QtShareName name QtNamePrefix + 'QFocusEvent_destroy'; 
function QFocusEvent_gotFocus(handle: QFocusEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFocusEvent_gotFocus';
function QFocusEvent_lostFocus(handle: QFocusEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFocusEvent_lostFocus';
function QFocusEvent_reason(handle: QFocusEventH): QtFocusReason; cdecl; external QtShareName name QtNamePrefix + 'QFocusEvent_reason';

function QPaintEvent_create(paintRegion: QRegionH): QPaintEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEvent_create';
procedure QPaintEvent_destroy(handle: QPaintEventH); cdecl; external QtShareName name QtNamePrefix + 'QPaintEvent_destroy'; 
function QPaintEvent_create(paintRect: PRect): QPaintEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEvent_create2';
procedure QPaintEvent_rect(handle: QPaintEventH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QPaintEvent_rect';
function QPaintEvent_region(handle: QPaintEventH): QRegionH; cdecl; external QtShareName name QtNamePrefix + 'QPaintEvent_region';

function QMoveEvent_create(pos: PPoint; oldPos: PPoint): QMoveEventH; cdecl; external QtShareName name QtNamePrefix + 'QMoveEvent_create';
procedure QMoveEvent_destroy(handle: QMoveEventH); cdecl; external QtShareName name QtNamePrefix + 'QMoveEvent_destroy'; 
function QMoveEvent_pos(handle: QMoveEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QMoveEvent_pos';
function QMoveEvent_oldPos(handle: QMoveEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QMoveEvent_oldPos';

function QResizeEvent_create(size: PSize; oldSize: PSize): QResizeEventH; cdecl; external QtShareName name QtNamePrefix + 'QResizeEvent_create';
procedure QResizeEvent_destroy(handle: QResizeEventH); cdecl; external QtShareName name QtNamePrefix + 'QResizeEvent_destroy'; 
function QResizeEvent_size(handle: QResizeEventH): PSize; cdecl; external QtShareName name QtNamePrefix + 'QResizeEvent_size';
function QResizeEvent_oldSize(handle: QResizeEventH): PSize; cdecl; external QtShareName name QtNamePrefix + 'QResizeEvent_oldSize';

function QCloseEvent_create(): QCloseEventH; cdecl; external QtShareName name QtNamePrefix + 'QCloseEvent_create';
procedure QCloseEvent_destroy(handle: QCloseEventH); cdecl; external QtShareName name QtNamePrefix + 'QCloseEvent_destroy'; 

function QIconDragEvent_create(): QIconDragEventH; cdecl; external QtShareName name QtNamePrefix + 'QIconDragEvent_create';
procedure QIconDragEvent_destroy(handle: QIconDragEventH); cdecl; external QtShareName name QtNamePrefix + 'QIconDragEvent_destroy'; 

function QShowEvent_create(): QShowEventH; cdecl; external QtShareName name QtNamePrefix + 'QShowEvent_create';
procedure QShowEvent_destroy(handle: QShowEventH); cdecl; external QtShareName name QtNamePrefix + 'QShowEvent_destroy'; 

function QHideEvent_create(): QHideEventH; cdecl; external QtShareName name QtNamePrefix + 'QHideEvent_create';
procedure QHideEvent_destroy(handle: QHideEventH); cdecl; external QtShareName name QtNamePrefix + 'QHideEvent_destroy'; 

function QContextMenuEvent_create(reason: QContextMenuEventReason; pos: PPoint; globalPos: PPoint): QContextMenuEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QContextMenuEvent_create';
procedure QContextMenuEvent_destroy(handle: QContextMenuEventH); cdecl; external QtShareName name QtNamePrefix + 'QContextMenuEvent_destroy'; 
function QContextMenuEvent_create(reason: QContextMenuEventReason; pos: PPoint): QContextMenuEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QContextMenuEvent_create2';
function QContextMenuEvent_x(handle: QContextMenuEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QContextMenuEvent_x';
function QContextMenuEvent_y(handle: QContextMenuEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QContextMenuEvent_y';
function QContextMenuEvent_globalX(handle: QContextMenuEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QContextMenuEvent_globalX';
function QContextMenuEvent_globalY(handle: QContextMenuEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QContextMenuEvent_globalY';
function QContextMenuEvent_pos(handle: QContextMenuEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QContextMenuEvent_pos';
function QContextMenuEvent_globalPos(handle: QContextMenuEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QContextMenuEvent_globalPos';
function QContextMenuEvent_reason(handle: QContextMenuEventH): QContextMenuEventReason; cdecl; external QtShareName name QtNamePrefix + 'QContextMenuEvent_reason';

function QInputMethodEvent_create(): QInputMethodEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QInputMethodEvent_create';
procedure QInputMethodEvent_destroy(handle: QInputMethodEventH); cdecl; external QtShareName name QtNamePrefix + 'QInputMethodEvent_destroy'; 
procedure QInputMethodEvent_setCommitString(handle: QInputMethodEventH; commitString: PWideString; replaceFrom: Integer = 0; replaceLength: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QInputMethodEvent_setCommitString';
procedure QInputMethodEvent_preeditString(handle: QInputMethodEventH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QInputMethodEvent_preeditString';
procedure QInputMethodEvent_commitString(handle: QInputMethodEventH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QInputMethodEvent_commitString';
function QInputMethodEvent_replacementStart(handle: QInputMethodEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QInputMethodEvent_replacementStart';
function QInputMethodEvent_replacementLength(handle: QInputMethodEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QInputMethodEvent_replacementLength';
function QInputMethodEvent_create(other: QInputMethodEventH): QInputMethodEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QInputMethodEvent_create3';

function QDropEvent_create(pos: PPoint; actions: QtDropActions; data: QMimeDataH; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers; _type: QEventType): QDropEventH; cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_create';
procedure QDropEvent_destroy(handle: QDropEventH); cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_destroy'; 
function QDropEvent_pos(handle: QDropEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_pos';
function QDropEvent_mouseButtons(handle: QDropEventH): QtMouseButtons; cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_mouseButtons';
function QDropEvent_keyboardModifiers(handle: QDropEventH): QtKeyboardModifiers; cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_keyboardModifiers';
function QDropEvent_possibleActions(handle: QDropEventH): QtDropActions; cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_possibleActions';
function QDropEvent_proposedAction(handle: QDropEventH): QtDropAction; cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_proposedAction';
procedure QDropEvent_acceptProposedAction(handle: QDropEventH); cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_acceptProposedAction';
function QDropEvent_dropAction(handle: QDropEventH): QtDropAction; cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_dropAction';
procedure QDropEvent_setDropAction(handle: QDropEventH; action: QtDropAction); cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_setDropAction';
function QDropEvent_source(handle: QDropEventH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_source';
function QDropEvent_mimeData(handle: QDropEventH): QMimeDataH; cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_mimeData';
function QDropEvent_format(handle: QDropEventH; n: Integer = 0): PAnsiChar; cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_format';
procedure QDropEvent_encodedData(handle: QDropEventH; retval: QByteArrayH; p1: PAnsiChar); cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_encodedData';
function QDropEvent_provides(handle: QDropEventH; p1: PAnsiChar): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QDropEvent_provides';

function QDragMoveEvent_create(pos: PPoint; actions: QtDropActions; data: QMimeDataH; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers; _type: QEventType): QDragMoveEventH; cdecl; external QtShareName name QtNamePrefix + 'QDragMoveEvent_create';
procedure QDragMoveEvent_destroy(handle: QDragMoveEventH); cdecl; external QtShareName name QtNamePrefix + 'QDragMoveEvent_destroy'; 
procedure QDragMoveEvent_answerRect(handle: QDragMoveEventH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QDragMoveEvent_answerRect';
procedure QDragMoveEvent_accept(handle: QDragMoveEventH); overload; cdecl; external QtShareName name QtNamePrefix + 'QDragMoveEvent_accept';
procedure QDragMoveEvent_ignore(handle: QDragMoveEventH); overload; cdecl; external QtShareName name QtNamePrefix + 'QDragMoveEvent_ignore';
procedure QDragMoveEvent_accept(handle: QDragMoveEventH; r: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QDragMoveEvent_accept2';
procedure QDragMoveEvent_ignore(handle: QDragMoveEventH; r: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QDragMoveEvent_ignore2';

function QDragEnterEvent_create(pos: PPoint; actions: QtDropActions; data: QMimeDataH; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers): QDragEnterEventH; cdecl; external QtShareName name QtNamePrefix + 'QDragEnterEvent_create';
procedure QDragEnterEvent_destroy(handle: QDragEnterEventH); cdecl; external QtShareName name QtNamePrefix + 'QDragEnterEvent_destroy'; 

function QDragLeaveEvent_create(): QDragLeaveEventH; cdecl; external QtShareName name QtNamePrefix + 'QDragLeaveEvent_create';
procedure QDragLeaveEvent_destroy(handle: QDragLeaveEventH); cdecl; external QtShareName name QtNamePrefix + 'QDragLeaveEvent_destroy'; 

function QHelpEvent_create(_type: QEventType; pos: PPoint; globalPos: PPoint): QHelpEventH; cdecl; external QtShareName name QtNamePrefix + 'QHelpEvent_create';
procedure QHelpEvent_destroy(handle: QHelpEventH); cdecl; external QtShareName name QtNamePrefix + 'QHelpEvent_destroy'; 
function QHelpEvent_x(handle: QHelpEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QHelpEvent_x';
function QHelpEvent_y(handle: QHelpEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QHelpEvent_y';
function QHelpEvent_globalX(handle: QHelpEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QHelpEvent_globalX';
function QHelpEvent_globalY(handle: QHelpEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QHelpEvent_globalY';
function QHelpEvent_pos(handle: QHelpEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QHelpEvent_pos';
function QHelpEvent_globalPos(handle: QHelpEventH): PPoint; cdecl; external QtShareName name QtNamePrefix + 'QHelpEvent_globalPos';

function QStatusTipEvent_create(tip: PWideString): QStatusTipEventH; cdecl; external QtShareName name QtNamePrefix + 'QStatusTipEvent_create';
procedure QStatusTipEvent_destroy(handle: QStatusTipEventH); cdecl; external QtShareName name QtNamePrefix + 'QStatusTipEvent_destroy'; 
procedure QStatusTipEvent_tip(handle: QStatusTipEventH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QStatusTipEvent_tip';

function QWhatsThisClickedEvent_create(href: PWideString): QWhatsThisClickedEventH; cdecl; external QtShareName name QtNamePrefix + 'QWhatsThisClickedEvent_create';
procedure QWhatsThisClickedEvent_destroy(handle: QWhatsThisClickedEventH); cdecl; external QtShareName name QtNamePrefix + 'QWhatsThisClickedEvent_destroy'; 
procedure QWhatsThisClickedEvent_href(handle: QWhatsThisClickedEventH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QWhatsThisClickedEvent_href';

function QActionEvent_create(_type: Integer; action: QActionH; before: QActionH = nil): QActionEventH; cdecl; external QtShareName name QtNamePrefix + 'QActionEvent_create';
procedure QActionEvent_destroy(handle: QActionEventH); cdecl; external QtShareName name QtNamePrefix + 'QActionEvent_destroy'; 
function QActionEvent_action(handle: QActionEventH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QActionEvent_action';
function QActionEvent_before(handle: QActionEventH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QActionEvent_before';

function QFileOpenEvent_create(_file: PWideString): QFileOpenEventH; cdecl; external QtShareName name QtNamePrefix + 'QFileOpenEvent_create';
procedure QFileOpenEvent_destroy(handle: QFileOpenEventH); cdecl; external QtShareName name QtNamePrefix + 'QFileOpenEvent_destroy'; 
procedure QFileOpenEvent_file(handle: QFileOpenEventH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFileOpenEvent_file';

function QShortcutEvent_create(key: QKeySequenceH; id: Integer; ambiguous: Boolean = False): QShortcutEventH; cdecl; external QtShareName name QtNamePrefix + 'QShortcutEvent_create';
procedure QShortcutEvent_destroy(handle: QShortcutEventH); cdecl; external QtShareName name QtNamePrefix + 'QShortcutEvent_destroy'; 
function QShortcutEvent_key(handle: QShortcutEventH): QKeySequenceH; cdecl; external QtShareName name QtNamePrefix + 'QShortcutEvent_key';
function QShortcutEvent_shortcutId(handle: QShortcutEventH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QShortcutEvent_shortcutId';
function QShortcutEvent_isAmbiguous(handle: QShortcutEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QShortcutEvent_isAmbiguous';

function QWindowStateChangeEvent_create(aOldState: QtWindowStates): QWindowStateChangeEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QWindowStateChangeEvent_create';
procedure QWindowStateChangeEvent_destroy(handle: QWindowStateChangeEventH); cdecl; external QtShareName name QtNamePrefix + 'QWindowStateChangeEvent_destroy'; 
function QWindowStateChangeEvent_create(aOldState: QtWindowStates; isOverride: Boolean): QWindowStateChangeEventH; overload; cdecl; external QtShareName name QtNamePrefix + 'QWindowStateChangeEvent_create2';
function QWindowStateChangeEvent_oldState(handle: QWindowStateChangeEventH): QtWindowStates; cdecl; external QtShareName name QtNamePrefix + 'QWindowStateChangeEvent_oldState';
function QWindowStateChangeEvent_isOverride(handle: QWindowStateChangeEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWindowStateChangeEvent_isOverride';

function QCursor_create(): QCursorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QCursor_create';
procedure QCursor_destroy(handle: QCursorH); cdecl; external QtShareName name QtNamePrefix + 'QCursor_destroy'; 
function QCursor_create(shape: QtCursorShape): QCursorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QCursor_create2';
function QCursor_create(bitmap: QBitmapH; mask: QBitmapH; hotX: Integer = -1; hotY: Integer = -1): QCursorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QCursor_create3';
function QCursor_create(pixmap: QPixmapH; hotX: Integer = -1; hotY: Integer = -1): QCursorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QCursor_create4';
function QCursor_create(cursor: QCursorH): QCursorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QCursor_create5';
function QCursor_shape(handle: QCursorH): QtCursorShape; cdecl; external QtShareName name QtNamePrefix + 'QCursor_shape';
procedure QCursor_setShape(handle: QCursorH; newShape: QtCursorShape); cdecl; external QtShareName name QtNamePrefix + 'QCursor_setShape';
function QCursor_bitmap(handle: QCursorH): QBitmapH; cdecl; external QtShareName name QtNamePrefix + 'QCursor_bitmap';
function QCursor_mask(handle: QCursorH): QBitmapH; cdecl; external QtShareName name QtNamePrefix + 'QCursor_mask';
procedure QCursor_pixmap(handle: QCursorH; retval: QPixmapH); cdecl; external QtShareName name QtNamePrefix + 'QCursor_pixmap';
procedure QCursor_hotSpot(handle: QCursorH; retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QCursor_hotSpot';
procedure QCursor_pos(retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QCursor_pos';
procedure QCursor_setPos(x: Integer; y: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QCursor_setPos';
procedure QCursor_setPos(p: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QCursor_setPos2';
function QCursor_handle(handle: QCursorH): HCURSOR; cdecl; external QtShareName name QtNamePrefix + 'QCursor_handle';
function QCursor_create(cursor: HCURSOR): QCursorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QCursor_create6';

function QGridLayout_create(parent: QWidgetH): QGridLayoutH; overload; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_create';
procedure QGridLayout_destroy(handle: QGridLayoutH); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_destroy'; 
function QGridLayout_create(): QGridLayoutH; overload; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_create2';
procedure QGridLayout_sizeHint(handle: QGridLayoutH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_sizeHint';
procedure QGridLayout_minimumSize(handle: QGridLayoutH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_minimumSize';
procedure QGridLayout_maximumSize(handle: QGridLayoutH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_maximumSize';
procedure QGridLayout_setRowStretch(handle: QGridLayoutH; row: Integer; stretch: Integer); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_setRowStretch';
procedure QGridLayout_setColumnStretch(handle: QGridLayoutH; column: Integer; stretch: Integer); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_setColumnStretch';
function QGridLayout_rowStretch(handle: QGridLayoutH; row: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_rowStretch';
function QGridLayout_columnStretch(handle: QGridLayoutH; column: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_columnStretch';
procedure QGridLayout_setRowMinimumHeight(handle: QGridLayoutH; row: Integer; minSize: Integer); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_setRowMinimumHeight';
procedure QGridLayout_setColumnMinimumWidth(handle: QGridLayoutH; column: Integer; minSize: Integer); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_setColumnMinimumWidth';
function QGridLayout_rowMinimumHeight(handle: QGridLayoutH; row: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_rowMinimumHeight';
function QGridLayout_columnMinimumWidth(handle: QGridLayoutH; column: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_columnMinimumWidth';
function QGridLayout_columnCount(handle: QGridLayoutH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_columnCount';
function QGridLayout_rowCount(handle: QGridLayoutH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_rowCount';
procedure QGridLayout_cellRect(handle: QGridLayoutH; retval: PRect; row: Integer; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_cellRect';
function QGridLayout_hasHeightForWidth(handle: QGridLayoutH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_hasHeightForWidth';
function QGridLayout_heightForWidth(handle: QGridLayoutH; p1: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_heightForWidth';
function QGridLayout_minimumHeightForWidth(handle: QGridLayoutH; p1: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_minimumHeightForWidth';
function QGridLayout_expandingDirections(handle: QGridLayoutH): QtOrientations; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_expandingDirections';
procedure QGridLayout_invalidate(handle: QGridLayoutH); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_invalidate';
procedure QGridLayout_addWidget(handle: QGridLayoutH; w: QWidgetH); overload; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_addWidget';
procedure QGridLayout_addWidget(handle: QGridLayoutH; p1: QWidgetH; row: Integer; column: Integer; p4: QtAlignment = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_addWidget2';
procedure QGridLayout_addWidget(handle: QGridLayoutH; p1: QWidgetH; row: Integer; column: Integer; rowSpan: Integer; columnSpan: Integer; p6: QtAlignment = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_addWidget3';
procedure QGridLayout_addLayout(handle: QGridLayoutH; p1: QLayoutH; row: Integer; column: Integer; p4: QtAlignment = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_addLayout';
procedure QGridLayout_addLayout(handle: QGridLayoutH; p1: QLayoutH; row: Integer; column: Integer; rowSpan: Integer; columnSpan: Integer; p6: QtAlignment = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_addLayout2';
procedure QGridLayout_setOriginCorner(handle: QGridLayoutH; p1: QtCorner); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_setOriginCorner';
function QGridLayout_originCorner(handle: QGridLayoutH): QtCorner; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_originCorner';
function QGridLayout_itemAt(handle: QGridLayoutH; p1: Integer): QLayoutItemH; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_itemAt';
function QGridLayout_takeAt(handle: QGridLayoutH; p1: Integer): QLayoutItemH; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_takeAt';
function QGridLayout_count(handle: QGridLayoutH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_count';
procedure QGridLayout_setGeometry(handle: QGridLayoutH; p1: PRect); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_setGeometry';
procedure QGridLayout_addItem(handle: QGridLayoutH; item: QLayoutItemH; row: Integer; column: Integer; rowSpan: Integer = 1; columnSpan: Integer = 1; p6: QtAlignment = 0); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_addItem';
procedure QGridLayout_setDefaultPositioning(handle: QGridLayoutH; n: Integer; orient: QtOrientation); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_setDefaultPositioning';
procedure QGridLayout_getItemPosition(handle: QGridLayoutH; idx: Integer; row: PInteger; column: PInteger; rowSpan: PInteger; columnSpan: PInteger); cdecl; external QtShareName name QtNamePrefix + 'QGridLayout_getItemPosition';


type
  QClipboardMode = ( // QClipboard::Mode (1)
    QClipboardClipboard, QClipboardSelection );

procedure QClipboard_clear(handle: QClipboardH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtShareName name QtNamePrefix + 'QClipboard_clear';
function QClipboard_supportsSelection(handle: QClipboardH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QClipboard_supportsSelection';
function QClipboard_ownsSelection(handle: QClipboardH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QClipboard_ownsSelection';
function QClipboard_ownsClipboard(handle: QClipboardH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QClipboard_ownsClipboard';
procedure QClipboard_text(handle: QClipboardH; retval: PWideString; mode: QClipboardMode = QClipboardClipboard); overload; cdecl; external QtShareName name QtNamePrefix + 'QClipboard_text';
procedure QClipboard_text(handle: QClipboardH; retval: PWideString; subtype: PWideString; mode: QClipboardMode = QClipboardClipboard); overload; cdecl; external QtShareName name QtNamePrefix + 'QClipboard_text2';
procedure QClipboard_setText(handle: QClipboardH; p1: PWideString; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtShareName name QtNamePrefix + 'QClipboard_setText';
function QClipboard_mimeData(handle: QClipboardH; mode: QClipboardMode = QClipboardClipboard): QMimeDataH; cdecl; external QtShareName name QtNamePrefix + 'QClipboard_mimeData';
procedure QClipboard_setMimeData(handle: QClipboardH; data: QMimeDataH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtShareName name QtNamePrefix + 'QClipboard_setMimeData';
procedure QClipboard_image(handle: QClipboardH; retval: QImageH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtShareName name QtNamePrefix + 'QClipboard_image';
procedure QClipboard_pixmap(handle: QClipboardH; retval: QPixmapH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtShareName name QtNamePrefix + 'QClipboard_pixmap';
procedure QClipboard_setImage(handle: QClipboardH; p1: QImageH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtShareName name QtNamePrefix + 'QClipboard_setImage';
procedure QClipboard_setPixmap(handle: QClipboardH; p1: QPixmapH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtShareName name QtNamePrefix + 'QClipboard_setPixmap';


type
  QClipboard_selectionChanged_Event = procedure () of object cdecl;
  QClipboard_dataChanged_Event = procedure () of object cdecl;


function QDesktopWidget_create(): QDesktopWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_create';
procedure QDesktopWidget_destroy(handle: QDesktopWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_destroy'; 
function QDesktopWidget_isVirtualDesktop(handle: QDesktopWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_isVirtualDesktop';
function QDesktopWidget_numScreens(handle: QDesktopWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_numScreens';
function QDesktopWidget_primaryScreen(handle: QDesktopWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_primaryScreen';
function QDesktopWidget_screenNumber(handle: QDesktopWidgetH; widget: QWidgetH = nil): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_screenNumber';
function QDesktopWidget_screenNumber(handle: QDesktopWidgetH; p1: PPoint): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_screenNumber2';
function QDesktopWidget_screen(handle: QDesktopWidgetH; screen: Integer = -1): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_screen';
procedure QDesktopWidget_screenGeometry(handle: QDesktopWidgetH; retval: PRect; screen: Integer = -1); overload; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_screenGeometry';
procedure QDesktopWidget_screenGeometry(handle: QDesktopWidgetH; retval: PRect; widget: QWidgetH); overload; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_screenGeometry2';
procedure QDesktopWidget_screenGeometry(handle: QDesktopWidgetH; retval: PRect; point: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_screenGeometry3';
procedure QDesktopWidget_availableGeometry(handle: QDesktopWidgetH; retval: PRect; screen: Integer = -1); overload; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_availableGeometry';
procedure QDesktopWidget_availableGeometry(handle: QDesktopWidgetH; retval: PRect; widget: QWidgetH); overload; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_availableGeometry2';
procedure QDesktopWidget_availableGeometry(handle: QDesktopWidgetH; retval: PRect; point: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_availableGeometry3';


type
  QDesktopWidget_resized_Event = procedure (p1: Integer) of object cdecl;
  QDesktopWidget_workAreaResized_Event = procedure (p1: Integer) of object cdecl;


procedure QToolTip_showText(pos: PPoint; text: PWideString; w: QWidgetH = nil); cdecl; external QtShareName name QtNamePrefix + 'QToolTip_showText';
procedure QToolTip_palette(retval: QPaletteH); cdecl; external QtShareName name QtNamePrefix + 'QToolTip_palette';


type
  QColorSpec = ( // QColor::Spec (1)
    QColorInvalid, QColorRgb, QColorHsv, QColorCmyk );

function QColor_create(): QColorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QColor_create';
procedure QColor_destroy(handle: QColorH); cdecl; external QtShareName name QtNamePrefix + 'QColor_destroy'; 
function QColor_create(color: QtGlobalColor): QColorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QColor_create2';
function QColor_create(r: Integer; g: Integer; b: Integer; a: Integer = 255): QColorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QColor_create3';
function QColor_create(rgb: QRgb): QColorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QColor_create4';
function QColor_create(name: PWideString): QColorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QColor_create5';
function QColor_create(name: PAnsiChar): QColorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QColor_create6';
function QColor_create(color: PQColor): QColorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QColor_create7';
function QColor_isValid(handle: QColorH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QColor_isValid';
procedure QColor_name(handle: QColorH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QColor_name';
procedure QColor_setNamedColor(handle: QColorH; name: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QColor_setNamedColor';
procedure QColor_colorNames(retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QColor_colorNames';
function QColor_spec(handle: QColorH): QColorSpec; cdecl; external QtShareName name QtNamePrefix + 'QColor_spec';
function QColor_alpha(handle: QColorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColor_alpha';
procedure QColor_setAlpha(handle: QColorH; alpha: Integer); cdecl; external QtShareName name QtNamePrefix + 'QColor_setAlpha';
function QColor_alphaF(handle: QColorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QColor_alphaF';
procedure QColor_setAlphaF(handle: QColorH; alpha: Double); cdecl; external QtShareName name QtNamePrefix + 'QColor_setAlphaF';
function QColor_red(handle: QColorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColor_red';
function QColor_green(handle: QColorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColor_green';
function QColor_blue(handle: QColorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColor_blue';
procedure QColor_setRed(handle: QColorH; red: Integer); cdecl; external QtShareName name QtNamePrefix + 'QColor_setRed';
procedure QColor_setGreen(handle: QColorH; green: Integer); cdecl; external QtShareName name QtNamePrefix + 'QColor_setGreen';
procedure QColor_setBlue(handle: QColorH; blue: Integer); cdecl; external QtShareName name QtNamePrefix + 'QColor_setBlue';
function QColor_redF(handle: QColorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QColor_redF';
function QColor_greenF(handle: QColorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QColor_greenF';
function QColor_blueF(handle: QColorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QColor_blueF';
procedure QColor_setRedF(handle: QColorH; red: Double); cdecl; external QtShareName name QtNamePrefix + 'QColor_setRedF';
procedure QColor_setGreenF(handle: QColorH; green: Double); cdecl; external QtShareName name QtNamePrefix + 'QColor_setGreenF';
procedure QColor_setBlueF(handle: QColorH; blue: Double); cdecl; external QtShareName name QtNamePrefix + 'QColor_setBlueF';
procedure QColor_getRgb(handle: QColorH; r: PInteger; g: PInteger; b: PInteger; a: PInteger = 0); cdecl; external QtShareName name QtNamePrefix + 'QColor_getRgb';
procedure QColor_setRgb(handle: QColorH; r: Integer; g: Integer; b: Integer; a: Integer = 255); overload; cdecl; external QtShareName name QtNamePrefix + 'QColor_setRgb';
procedure QColor_getRgbF(handle: QColorH; r: PDouble; g: PDouble; b: PDouble; a: PDouble = 0); cdecl; external QtShareName name QtNamePrefix + 'QColor_getRgbF';
procedure QColor_setRgbF(handle: QColorH; r: Double; g: Double; b: Double; a: Double = 1.0); cdecl; external QtShareName name QtNamePrefix + 'QColor_setRgbF';
function QColor_rgba(handle: QColorH): QRgb; cdecl; external QtShareName name QtNamePrefix + 'QColor_rgba';
procedure QColor_setRgba(handle: QColorH; rgba: QRgb); cdecl; external QtShareName name QtNamePrefix + 'QColor_setRgba';
function QColor_rgb(handle: QColorH): QRgb; cdecl; external QtShareName name QtNamePrefix + 'QColor_rgb';
procedure QColor_setRgb(handle: QColorH; rgb: QRgb); overload; cdecl; external QtShareName name QtNamePrefix + 'QColor_setRgb2';
function QColor_hue(handle: QColorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColor_hue';
function QColor_saturation(handle: QColorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColor_saturation';
function QColor_value(handle: QColorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColor_value';
function QColor_hueF(handle: QColorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QColor_hueF';
function QColor_saturationF(handle: QColorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QColor_saturationF';
function QColor_valueF(handle: QColorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QColor_valueF';
procedure QColor_getHsv(handle: QColorH; h: PInteger; s: PInteger; v: PInteger; a: PInteger = 0); cdecl; external QtShareName name QtNamePrefix + 'QColor_getHsv';
procedure QColor_setHsv(handle: QColorH; h: Integer; s: Integer; v: Integer; a: Integer = 255); cdecl; external QtShareName name QtNamePrefix + 'QColor_setHsv';
procedure QColor_getHsvF(handle: QColorH; h: PDouble; s: PDouble; v: PDouble; a: PDouble = 0); cdecl; external QtShareName name QtNamePrefix + 'QColor_getHsvF';
procedure QColor_setHsvF(handle: QColorH; h: Double; s: Double; v: Double; a: Double = 1.0); cdecl; external QtShareName name QtNamePrefix + 'QColor_setHsvF';
function QColor_cyan(handle: QColorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColor_cyan';
function QColor_magenta(handle: QColorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColor_magenta';
function QColor_yellow(handle: QColorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColor_yellow';
function QColor_black(handle: QColorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColor_black';
function QColor_cyanF(handle: QColorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QColor_cyanF';
function QColor_magentaF(handle: QColorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QColor_magentaF';
function QColor_yellowF(handle: QColorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QColor_yellowF';
function QColor_blackF(handle: QColorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QColor_blackF';
procedure QColor_getCmyk(handle: QColorH; c: PInteger; m: PInteger; y: PInteger; k: PInteger; a: PInteger = 0); cdecl; external QtShareName name QtNamePrefix + 'QColor_getCmyk';
procedure QColor_setCmyk(handle: QColorH; c: Integer; m: Integer; y: Integer; k: Integer; a: Integer = 255); cdecl; external QtShareName name QtNamePrefix + 'QColor_setCmyk';
procedure QColor_getCmykF(handle: QColorH; c: PDouble; m: PDouble; y: PDouble; k: PDouble; a: PDouble = 0); cdecl; external QtShareName name QtNamePrefix + 'QColor_getCmykF';
procedure QColor_setCmykF(handle: QColorH; c: Double; m: Double; y: Double; k: Double; a: Double = 1.0); cdecl; external QtShareName name QtNamePrefix + 'QColor_setCmykF';
procedure QColor_toRgb(handle: QColorH; retval: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QColor_toRgb';
procedure QColor_toHsv(handle: QColorH; retval: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QColor_toHsv';
procedure QColor_toCmyk(handle: QColorH; retval: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QColor_toCmyk';
procedure QColor_convertTo(handle: QColorH; retval: PQColor; colorSpec: QColorSpec); cdecl; external QtShareName name QtNamePrefix + 'QColor_convertTo';
procedure QColor_fromRgb(retval: PQColor; rgb: QRgb); overload; cdecl; external QtShareName name QtNamePrefix + 'QColor_fromRgb';
procedure QColor_fromRgba(retval: PQColor; rgba: QRgb); cdecl; external QtShareName name QtNamePrefix + 'QColor_fromRgba';
procedure QColor_fromRgb(retval: PQColor; r: Integer; g: Integer; b: Integer; a: Integer = 255); overload; cdecl; external QtShareName name QtNamePrefix + 'QColor_fromRgb2';
procedure QColor_fromRgbF(retval: PQColor; r: Double; g: Double; b: Double; a: Double = 1.0); cdecl; external QtShareName name QtNamePrefix + 'QColor_fromRgbF';
procedure QColor_fromHsv(retval: PQColor; h: Integer; s: Integer; v: Integer; a: Integer = 255); cdecl; external QtShareName name QtNamePrefix + 'QColor_fromHsv';
procedure QColor_fromHsvF(retval: PQColor; h: Double; s: Double; v: Double; a: Double = 1.0); cdecl; external QtShareName name QtNamePrefix + 'QColor_fromHsvF';
procedure QColor_fromCmyk(retval: PQColor; c: Integer; m: Integer; y: Integer; k: Integer; a: Integer = 255); cdecl; external QtShareName name QtNamePrefix + 'QColor_fromCmyk';
procedure QColor_fromCmykF(retval: PQColor; c: Double; m: Double; y: Double; k: Double; a: Double = 1.0); cdecl; external QtShareName name QtNamePrefix + 'QColor_fromCmykF';
procedure QColor_light(handle: QColorH; retval: PQColor; f: Integer = 150); cdecl; external QtShareName name QtNamePrefix + 'QColor_light';
procedure QColor_dark(handle: QColorH; retval: PQColor; f: Integer = 200); cdecl; external QtShareName name QtNamePrefix + 'QColor_dark';

function QMatrix_create(): QMatrixH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_create';
procedure QMatrix_destroy(handle: QMatrixH); cdecl; external QtShareName name QtNamePrefix + 'QMatrix_destroy'; 
function QMatrix_create(m11: Double; m12: Double; m21: Double; m22: Double; dx: Double; dy: Double): QMatrixH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_create2';
function QMatrix_create(matrix: QMatrixH): QMatrixH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_create3';
procedure QMatrix_setMatrix(handle: QMatrixH; m11: Double; m12: Double; m21: Double; m22: Double; dx: Double; dy: Double); cdecl; external QtShareName name QtNamePrefix + 'QMatrix_setMatrix';
function QMatrix_m11(handle: QMatrixH): Double; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_m11';
function QMatrix_m12(handle: QMatrixH): Double; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_m12';
function QMatrix_m21(handle: QMatrixH): Double; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_m21';
function QMatrix_m22(handle: QMatrixH): Double; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_m22';
function QMatrix_dx(handle: QMatrixH): Double; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_dx';
function QMatrix_dy(handle: QMatrixH): Double; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_dy';
procedure QMatrix_map(handle: QMatrixH; x: Integer; y: Integer; tx: PInteger; ty: PInteger); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_map';
procedure QMatrix_map(handle: QMatrixH; x: Double; y: Double; tx: PDouble; ty: PDouble); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_map2';
procedure QMatrix_mapRect(handle: QMatrixH; retval: PRect; p1: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_mapRect';
procedure QMatrix_mapRect(handle: QMatrixH; retval: QRectFH; p1: QRectFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_mapRect2';
procedure QMatrix_map(handle: QMatrixH; retval: PPoint; p: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_map3';
procedure QMatrix_map(handle: QMatrixH; retval: QPointFH; p: QPointFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_map4';
procedure QMatrix_map(handle: QMatrixH; retval: QLineH; l: QLineH); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_map5';
procedure QMatrix_map(handle: QMatrixH; retval: QLineFH; l: QLineFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_map6';
procedure QMatrix_map(handle: QMatrixH; retval: QPolygonFH; a: QPolygonFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_map7';
procedure QMatrix_map(handle: QMatrixH; retval: QPolygonH; a: QPolygonH); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_map8';
procedure QMatrix_map(handle: QMatrixH; retval: QRegionH; r: QRegionH); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_map9';
procedure QMatrix_map(handle: QMatrixH; retval: QPainterPathH; p: QPainterPathH); overload; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_map10';
procedure QMatrix_mapToPolygon(handle: QMatrixH; retval: QPolygonH; r: PRect); cdecl; external QtShareName name QtNamePrefix + 'QMatrix_mapToPolygon';
procedure QMatrix_reset(handle: QMatrixH); cdecl; external QtShareName name QtNamePrefix + 'QMatrix_reset';
function QMatrix_isIdentity(handle: QMatrixH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_isIdentity';
function QMatrix_translate(handle: QMatrixH; dx: Double; dy: Double): QMatrixH; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_translate';
function QMatrix_scale(handle: QMatrixH; sx: Double; sy: Double): QMatrixH; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_scale';
function QMatrix_shear(handle: QMatrixH; sh: Double; sv: Double): QMatrixH; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_shear';
function QMatrix_rotate(handle: QMatrixH; a: Double): QMatrixH; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_rotate';
function QMatrix_isInvertible(handle: QMatrixH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_isInvertible';
function QMatrix_det(handle: QMatrixH): Double; cdecl; external QtShareName name QtNamePrefix + 'QMatrix_det';
procedure QMatrix_inverted(handle: QMatrixH; retval: QMatrixH; invertible: PBoolean = 0); cdecl; external QtShareName name QtNamePrefix + 'QMatrix_inverted';


type
  QGradientType = ( // QGradient::Type (1)
    QGradientLinearGradient, QGradientRadialGradient, QGradientConicalGradient, QGradientNoGradient );

  QGradientSpread = ( // QGradient::Spread (1)
    QGradientPadSpread, QGradientReflectSpread, QGradientRepeatSpread );

function QBrush_create(): QBrushH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBrush_create';
procedure QBrush_destroy(handle: QBrushH); cdecl; external QtShareName name QtNamePrefix + 'QBrush_destroy'; 
function QBrush_create(bs: QtBrushStyle): QBrushH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBrush_create2';
function QBrush_create(color: PQColor; bs: QtBrushStyle = QtSolidPattern): QBrushH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBrush_create3';
function QBrush_create(color: QtGlobalColor; bs: QtBrushStyle = QtSolidPattern): QBrushH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBrush_create4';
function QBrush_create(color: PQColor; pixmap: QPixmapH): QBrushH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBrush_create5';
function QBrush_create(color: QtGlobalColor; pixmap: QPixmapH): QBrushH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBrush_create6';
function QBrush_create(pixmap: QPixmapH): QBrushH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBrush_create7';
function QBrush_create(brush: QBrushH): QBrushH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBrush_create8';
function QBrush_create(gradient: QGradientH): QBrushH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBrush_create9';
function QBrush_style(handle: QBrushH): QtBrushStyle; cdecl; external QtShareName name QtNamePrefix + 'QBrush_style';
procedure QBrush_setStyle(handle: QBrushH; p1: QtBrushStyle); cdecl; external QtShareName name QtNamePrefix + 'QBrush_setStyle';
procedure QBrush_texture(handle: QBrushH; retval: QPixmapH); cdecl; external QtShareName name QtNamePrefix + 'QBrush_texture';
procedure QBrush_setTexture(handle: QBrushH; pixmap: QPixmapH); cdecl; external QtShareName name QtNamePrefix + 'QBrush_setTexture';
function QBrush_color(handle: QBrushH): PQColor; cdecl; external QtShareName name QtNamePrefix + 'QBrush_color';
procedure QBrush_setColor(handle: QBrushH; color: PQColor); overload; cdecl; external QtShareName name QtNamePrefix + 'QBrush_setColor';
procedure QBrush_setColor(handle: QBrushH; color: QtGlobalColor); overload; cdecl; external QtShareName name QtNamePrefix + 'QBrush_setColor2';
function QBrush_gradient(handle: QBrushH): QGradientH; cdecl; external QtShareName name QtNamePrefix + 'QBrush_gradient';
function QBrush_isOpaque(handle: QBrushH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QBrush_isOpaque';

function QGradient_create(): QGradientH; cdecl; external QtShareName name QtNamePrefix + 'QGradient_create';
procedure QGradient_destroy(handle: QGradientH); cdecl; external QtShareName name QtNamePrefix + 'QGradient_destroy'; 
function QGradient_type(handle: QGradientH): QGradientType; cdecl; external QtShareName name QtNamePrefix + 'QGradient_type';
procedure QGradient_setSpread(handle: QGradientH; spread: QGradientSpread); cdecl; external QtShareName name QtNamePrefix + 'QGradient_setSpread';
function QGradient_spread(handle: QGradientH): QGradientSpread; cdecl; external QtShareName name QtNamePrefix + 'QGradient_spread';
procedure QGradient_setColorAt(handle: QGradientH; pos: Double; color: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QGradient_setColorAt';

function QLinearGradient_create(start: QPointFH; finalStop: QPointFH): QLinearGradientH; overload; cdecl; external QtShareName name QtNamePrefix + 'QLinearGradient_create';
procedure QLinearGradient_destroy(handle: QLinearGradientH); cdecl; external QtShareName name QtNamePrefix + 'QLinearGradient_destroy'; 
function QLinearGradient_create(xStart: Double; yStart: Double; xFinalStop: Double; yFinalStop: Double): QLinearGradientH; overload; cdecl; external QtShareName name QtNamePrefix + 'QLinearGradient_create2';
procedure QLinearGradient_start(handle: QLinearGradientH; retval: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QLinearGradient_start';
procedure QLinearGradient_finalStop(handle: QLinearGradientH; retval: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QLinearGradient_finalStop';

function QRadialGradient_create(center: QPointFH; radius: Double; focalPoint: QPointFH = nil): QRadialGradientH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRadialGradient_create';
procedure QRadialGradient_destroy(handle: QRadialGradientH); cdecl; external QtShareName name QtNamePrefix + 'QRadialGradient_destroy'; 
function QRadialGradient_create(cx: Double; cy: Double; radius: Double; fx: Double = 0; fy: Double = 0): QRadialGradientH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRadialGradient_create2';
procedure QRadialGradient_center(handle: QRadialGradientH; retval: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRadialGradient_center';
procedure QRadialGradient_focalPoint(handle: QRadialGradientH; retval: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QRadialGradient_focalPoint';
function QRadialGradient_radius(handle: QRadialGradientH): Double; cdecl; external QtShareName name QtNamePrefix + 'QRadialGradient_radius';

function QConicalGradient_create(center: QPointFH; startAngle: Double): QConicalGradientH; overload; cdecl; external QtShareName name QtNamePrefix + 'QConicalGradient_create';
procedure QConicalGradient_destroy(handle: QConicalGradientH); cdecl; external QtShareName name QtNamePrefix + 'QConicalGradient_destroy'; 
function QConicalGradient_create(cx: Double; cy: Double; startAngle: Double): QConicalGradientH; overload; cdecl; external QtShareName name QtNamePrefix + 'QConicalGradient_create2';
procedure QConicalGradient_center(handle: QConicalGradientH; retval: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QConicalGradient_center';
function QConicalGradient_angle(handle: QConicalGradientH): Double; cdecl; external QtShareName name QtNamePrefix + 'QConicalGradient_angle';

function QPen_create(): QPenH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPen_create';
procedure QPen_destroy(handle: QPenH); cdecl; external QtShareName name QtNamePrefix + 'QPen_destroy'; 
function QPen_create(p1: QtPenStyle): QPenH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPen_create2';
function QPen_create(color: PQColor): QPenH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPen_create3';
function QPen_create(brush: QBrushH; width: Double; s: QtPenStyle = QtSolidLine; c: QtPenCapStyle = QtSquareCap; j: QtPenJoinStyle = QtBevelJoin): QPenH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPen_create4';
function QPen_create(pen: QPenH): QPenH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPen_create5';
function QPen_style(handle: QPenH): QtPenStyle; cdecl; external QtShareName name QtNamePrefix + 'QPen_style';
procedure QPen_setStyle(handle: QPenH; p1: QtPenStyle); cdecl; external QtShareName name QtNamePrefix + 'QPen_setStyle';
function QPen_miterLimit(handle: QPenH): Double; cdecl; external QtShareName name QtNamePrefix + 'QPen_miterLimit';
procedure QPen_setMiterLimit(handle: QPenH; limit: Double); cdecl; external QtShareName name QtNamePrefix + 'QPen_setMiterLimit';
function QPen_widthF(handle: QPenH): Double; cdecl; external QtShareName name QtNamePrefix + 'QPen_widthF';
procedure QPen_setWidthF(handle: QPenH; width: Double); cdecl; external QtShareName name QtNamePrefix + 'QPen_setWidthF';
function QPen_width(handle: QPenH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPen_width';
procedure QPen_setWidth(handle: QPenH; width: Integer); cdecl; external QtShareName name QtNamePrefix + 'QPen_setWidth';
procedure QPen_color(handle: QPenH; retval: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QPen_color';
procedure QPen_setColor(handle: QPenH; color: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QPen_setColor';
procedure QPen_brush(handle: QPenH; retval: QBrushH); cdecl; external QtShareName name QtNamePrefix + 'QPen_brush';
procedure QPen_setBrush(handle: QPenH; brush: QBrushH); cdecl; external QtShareName name QtNamePrefix + 'QPen_setBrush';
function QPen_isSolid(handle: QPenH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPen_isSolid';
function QPen_capStyle(handle: QPenH): QtPenCapStyle; cdecl; external QtShareName name QtNamePrefix + 'QPen_capStyle';
procedure QPen_setCapStyle(handle: QPenH; pcs: QtPenCapStyle); cdecl; external QtShareName name QtNamePrefix + 'QPen_setCapStyle';
function QPen_joinStyle(handle: QPenH): QtPenJoinStyle; cdecl; external QtShareName name QtNamePrefix + 'QPen_joinStyle';
procedure QPen_setJoinStyle(handle: QPenH; pcs: QtPenJoinStyle); cdecl; external QtShareName name QtNamePrefix + 'QPen_setJoinStyle';
function QPen_isDetached(handle: QPenH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPen_isDetached';

function QPolygon_create(): QPolygonH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_create';
procedure QPolygon_destroy(handle: QPolygonH); cdecl; external QtShareName name QtNamePrefix + 'QPolygon_destroy'; 
function QPolygon_create(size: Integer): QPolygonH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_create2';
function QPolygon_create(a: QPolygonH): QPolygonH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_create3';
function QPolygon_create(r: PRect; closed: Boolean = False): QPolygonH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_create4';
function QPolygon_create(nPoints: Integer; points: PInteger): QPolygonH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_create5';
procedure QPolygon_translate(handle: QPolygonH; dx: Integer; dy: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_translate';
procedure QPolygon_translate(handle: QPolygonH; offset: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_translate2';
procedure QPolygon_boundingRect(handle: QPolygonH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QPolygon_boundingRect';
procedure QPolygon_point(handle: QPolygonH; i: Integer; x: PInteger; y: PInteger); overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_point';
procedure QPolygon_point(handle: QPolygonH; retval: PPoint; i: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_point2';
procedure QPolygon_setPoint(handle: QPolygonH; index: Integer; x: Integer; y: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_setPoint';
procedure QPolygon_setPoint(handle: QPolygonH; index: Integer; p: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_setPoint2';
procedure QPolygon_setPoints(handle: QPolygonH; nPoints: Integer; points: PInteger); overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_setPoints';
procedure QPolygon_putPoints(handle: QPolygonH; index: Integer; nPoints: Integer; points: PInteger); overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_putPoints';
procedure QPolygon_putPoints(handle: QPolygonH; index: Integer; nPoints: Integer; from: QPolygonH; fromIndex: Integer = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygon_putPoints3';

function QPolygonF_create(): QPolygonFH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygonF_create';
procedure QPolygonF_destroy(handle: QPolygonFH); cdecl; external QtShareName name QtNamePrefix + 'QPolygonF_destroy'; 
function QPolygonF_create(size: Integer): QPolygonFH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygonF_create2';
function QPolygonF_create(a: QPolygonFH): QPolygonFH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygonF_create3';
function QPolygonF_create(r: QRectFH): QPolygonFH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygonF_create4';
function QPolygonF_create(a: QPolygonH): QPolygonFH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygonF_create5';
procedure QPolygonF_translate(handle: QPolygonFH; dx: Double; dy: Double); overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygonF_translate';
procedure QPolygonF_translate(handle: QPolygonFH; offset: QPointFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPolygonF_translate2';
procedure QPolygonF_toPolygon(handle: QPolygonFH; retval: QPolygonH); cdecl; external QtShareName name QtNamePrefix + 'QPolygonF_toPolygon';
function QPolygonF_isClosed(handle: QPolygonFH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPolygonF_isClosed';
procedure QPolygonF_boundingRect(handle: QPolygonFH; retval: QRectFH); cdecl; external QtShareName name QtNamePrefix + 'QPolygonF_boundingRect';


type
  QPainterCompositionMode = ( // QPainter::CompositionMode (1)
    QPainterCompositionMode_SourceOver, QPainterCompositionMode_DestinationOver, QPainterCompositionMode_Clear, QPainterCompositionMode_Source, QPainterCompositionMode_Destination, QPainterCompositionMode_SourceIn, 
    QPainterCompositionMode_DestinationIn, QPainterCompositionMode_SourceOut, QPainterCompositionMode_DestinationOut, QPainterCompositionMode_SourceAtop, QPainterCompositionMode_DestinationAtop, 
    QPainterCompositionMode_Xor );

type
  QPainterRenderHint = cardinal; // QPainter::RenderHint
  QPainterRenderHints = QPainterRenderHint; //QFlags<> (3)
const
  QPainterAntialiasing =   $01;
  QPainterTextAntialiasing =   $02;
  QPainterSmoothPixmapTransform =   $04;

function QPainter_create(): QPainterH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_create';
procedure QPainter_destroy(handle: QPainterH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_destroy'; 
function QPainter_create(p1: QPaintDeviceH): QPainterH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_create2';
function QPainter_device(handle: QPainterH): QPaintDeviceH; cdecl; external QtShareName name QtNamePrefix + 'QPainter_device';
function QPainter_begin(handle: QPainterH; p1: QPaintDeviceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPainter_begin';
function QPainter_end(handle: QPainterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPainter_end';
function QPainter_isActive(handle: QPainterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPainter_isActive';
procedure QPainter_initFrom(handle: QPainterH; widget: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_initFrom';
procedure QPainter_setCompositionMode(handle: QPainterH; mode: QPainterCompositionMode); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setCompositionMode';
function QPainter_compositionMode(handle: QPainterH): QPainterCompositionMode; cdecl; external QtShareName name QtNamePrefix + 'QPainter_compositionMode';
function QPainter_font(handle: QPainterH): QFontH; cdecl; external QtShareName name QtNamePrefix + 'QPainter_font';
procedure QPainter_setFont(handle: QPainterH; f: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setFont';
procedure QPainter_fontMetrics(handle: QPainterH; retval: QFontMetricsH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_fontMetrics';
procedure QPainter_fontInfo(handle: QPainterH; retval: QFontInfoH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_fontInfo';
procedure QPainter_setPen(handle: QPainterH; color: PQColor); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setPen';
procedure QPainter_setPen(handle: QPainterH; pen: QPenH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setPen2';
procedure QPainter_setPen(handle: QPainterH; style: QtPenStyle); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setPen3';
function QPainter_pen(handle: QPainterH): QPenH; cdecl; external QtShareName name QtNamePrefix + 'QPainter_pen';
procedure QPainter_setBrush(handle: QPainterH; brush: QBrushH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setBrush';
procedure QPainter_setBrush(handle: QPainterH; style: QtBrushStyle); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setBrush2';
function QPainter_brush(handle: QPainterH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPainter_brush';
procedure QPainter_setBackgroundMode(handle: QPainterH; mode: QtBGMode); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setBackgroundMode';
function QPainter_backgroundMode(handle: QPainterH): QtBGMode; cdecl; external QtShareName name QtNamePrefix + 'QPainter_backgroundMode';
procedure QPainter_brushOrigin(handle: QPainterH; retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QPainter_brushOrigin';
procedure QPainter_setBrushOrigin(handle: QPainterH; x: Integer; y: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setBrushOrigin';
procedure QPainter_setBrushOrigin(handle: QPainterH; p1: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setBrushOrigin2';
procedure QPainter_setBrushOrigin(handle: QPainterH; p1: QPointFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setBrushOrigin3';
procedure QPainter_setBackground(handle: QPainterH; bg: QBrushH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setBackground';
function QPainter_background(handle: QPainterH): QBrushH; cdecl; external QtShareName name QtNamePrefix + 'QPainter_background';
procedure QPainter_clipRegion(handle: QPainterH; retval: QRegionH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_clipRegion';
procedure QPainter_clipPath(handle: QPainterH; retval: QPainterPathH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_clipPath';
procedure QPainter_setClipRect(handle: QPainterH; p1: QRectFH; op: QtClipOperation = QtReplaceClip); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setClipRect';
procedure QPainter_setClipRect(handle: QPainterH; p1: PRect; op: QtClipOperation = QtReplaceClip); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setClipRect2';
procedure QPainter_setClipRect(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; op: QtClipOperation = QtReplaceClip); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setClipRect3';
procedure QPainter_setClipRegion(handle: QPainterH; p1: QRegionH; op: QtClipOperation = QtReplaceClip); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setClipRegion';
procedure QPainter_setClipPath(handle: QPainterH; path: QPainterPathH; op: QtClipOperation = QtReplaceClip); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setClipPath';
procedure QPainter_setClipping(handle: QPainterH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setClipping';
function QPainter_hasClipping(handle: QPainterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPainter_hasClipping';
procedure QPainter_save(handle: QPainterH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_save';
procedure QPainter_restore(handle: QPainterH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_restore';
procedure QPainter_setMatrix(handle: QPainterH; matrix: QMatrixH; combine: Boolean = False); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setMatrix';
function QPainter_matrix(handle: QPainterH): QMatrixH; cdecl; external QtShareName name QtNamePrefix + 'QPainter_matrix';
function QPainter_deviceMatrix(handle: QPainterH): QMatrixH; cdecl; external QtShareName name QtNamePrefix + 'QPainter_deviceMatrix';
procedure QPainter_resetMatrix(handle: QPainterH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_resetMatrix';
procedure QPainter_setMatrixEnabled(handle: QPainterH; enabled: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setMatrixEnabled';
function QPainter_matrixEnabled(handle: QPainterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPainter_matrixEnabled';
procedure QPainter_scale(handle: QPainterH; sx: Double; sy: Double); cdecl; external QtShareName name QtNamePrefix + 'QPainter_scale';
procedure QPainter_shear(handle: QPainterH; sh: Double; sv: Double); cdecl; external QtShareName name QtNamePrefix + 'QPainter_shear';
procedure QPainter_rotate(handle: QPainterH; a: Double); cdecl; external QtShareName name QtNamePrefix + 'QPainter_rotate';
procedure QPainter_translate(handle: QPainterH; offset: QPointFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_translate';
procedure QPainter_translate(handle: QPainterH; offset: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_translate2';
procedure QPainter_translate(handle: QPainterH; dx: Double; dy: Double); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_translate3';
procedure QPainter_window(handle: QPainterH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QPainter_window';
procedure QPainter_setWindow(handle: QPainterH; window: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setWindow';
procedure QPainter_setWindow(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setWindow2';
procedure QPainter_viewport(handle: QPainterH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QPainter_viewport';
procedure QPainter_setViewport(handle: QPainterH; viewport: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setViewport';
procedure QPainter_setViewport(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_setViewport2';
procedure QPainter_setViewTransformEnabled(handle: QPainterH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setViewTransformEnabled';
function QPainter_viewTransformEnabled(handle: QPainterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPainter_viewTransformEnabled';
procedure QPainter_strokePath(handle: QPainterH; path: QPainterPathH; pen: QPenH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_strokePath';
procedure QPainter_fillPath(handle: QPainterH; path: QPainterPathH; brush: QBrushH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_fillPath';
procedure QPainter_drawPath(handle: QPainterH; path: QPainterPathH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPath';
procedure QPainter_drawPoint(handle: QPainterH; pt: QPointFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPoint';
procedure QPainter_drawPoint(handle: QPainterH; p: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPoint2';
procedure QPainter_drawPoint(handle: QPainterH; x: Integer; y: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPoint3';
procedure QPainter_drawPoints(handle: QPainterH; points: QPointFH; pointCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPoints';
procedure QPainter_drawPoints(handle: QPainterH; points: QPolygonFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPoints2';
procedure QPainter_drawPoints(handle: QPainterH; points: PPoint; pointCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPoints3';
procedure QPainter_drawPoints(handle: QPainterH; points: QPolygonH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPoints4';
procedure QPainter_drawLine(handle: QPainterH; line: QLineFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawLine';
procedure QPainter_drawLine(handle: QPainterH; line: QLineH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawLine2';
procedure QPainter_drawLine(handle: QPainterH; x1: Integer; y1: Integer; x2: Integer; y2: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawLine3';
procedure QPainter_drawLine(handle: QPainterH; p1: PPoint; p2: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawLine4';
procedure QPainter_drawLine(handle: QPainterH; p1: QPointFH; p2: QPointFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawLine5';
procedure QPainter_drawLines(handle: QPainterH; lines: QLineFH; lineCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawLines';
procedure QPainter_drawLines(handle: QPainterH; pointPairs: QPointFH; lineCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawLines2';
procedure QPainter_drawLines(handle: QPainterH; lines: QLineH; lineCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawLines3';
procedure QPainter_drawLines(handle: QPainterH; pointPairs: PPoint; lineCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawLines4';
procedure QPainter_drawRect(handle: QPainterH; rect: QRectFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawRect';
procedure QPainter_drawRect(handle: QPainterH; x1: Integer; y1: Integer; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawRect2';
procedure QPainter_drawRect(handle: QPainterH; rect: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawRect3';
procedure QPainter_drawRects(handle: QPainterH; rects: QRectFH; rectCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawRects';
procedure QPainter_drawRects(handle: QPainterH; rects: PRect; rectCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawRects2';
procedure QPainter_drawEllipse(handle: QPainterH; r: QRectFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawEllipse';
procedure QPainter_drawEllipse(handle: QPainterH; r: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawEllipse2';
procedure QPainter_drawEllipse(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawEllipse3';
procedure QPainter_drawPolyline(handle: QPainterH; points: QPointFH; pointCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPolyline';
procedure QPainter_drawPolyline(handle: QPainterH; polyline: QPolygonFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPolyline2';
procedure QPainter_drawPolyline(handle: QPainterH; points: PPoint; pointCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPolyline3';
procedure QPainter_drawPolyline(handle: QPainterH; polygon: QPolygonH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPolyline4';
procedure QPainter_drawPolygon(handle: QPainterH; points: QPointFH; pointCount: Integer; fillRule: QtFillRule = QtOddEvenFill); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPolygon';
procedure QPainter_drawPolygon(handle: QPainterH; polygon: QPolygonFH; fillRule: QtFillRule = QtOddEvenFill); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPolygon2';
procedure QPainter_drawPolygon(handle: QPainterH; points: PPoint; pointCount: Integer; fillRule: QtFillRule = QtOddEvenFill); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPolygon3';
procedure QPainter_drawPolygon(handle: QPainterH; polygon: QPolygonH; fillRule: QtFillRule = QtOddEvenFill); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPolygon4';
procedure QPainter_drawConvexPolygon(handle: QPainterH; points: QPointFH; pointCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawConvexPolygon';
procedure QPainter_drawConvexPolygon(handle: QPainterH; polygon: QPolygonFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawConvexPolygon2';
procedure QPainter_drawConvexPolygon(handle: QPainterH; points: PPoint; pointCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawConvexPolygon3';
procedure QPainter_drawConvexPolygon(handle: QPainterH; polygon: QPolygonH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawConvexPolygon4';
procedure QPainter_drawArc(handle: QPainterH; rect: QRectFH; a: Integer; alen: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawArc';
procedure QPainter_drawArc(handle: QPainterH; p1: PRect; a: Integer; alen: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawArc2';
procedure QPainter_drawArc(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; a: Integer; alen: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawArc3';
procedure QPainter_drawPie(handle: QPainterH; rect: QRectFH; a: Integer; alen: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPie';
procedure QPainter_drawPie(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; a: Integer; alen: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPie2';
procedure QPainter_drawPie(handle: QPainterH; p1: PRect; a: Integer; alen: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPie3';
procedure QPainter_drawChord(handle: QPainterH; rect: QRectFH; a: Integer; alen: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawChord';
procedure QPainter_drawChord(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; a: Integer; alen: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawChord2';
procedure QPainter_drawChord(handle: QPainterH; p1: PRect; a: Integer; alen: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawChord3';
procedure QPainter_drawRoundRect(handle: QPainterH; r: QRectFH; xround: Integer = 25; yround: Integer = 25); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawRoundRect';
procedure QPainter_drawRoundRect(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; p5: Integer = 25; p6: Integer = 25); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawRoundRect2';
procedure QPainter_drawRoundRect(handle: QPainterH; r: PRect; xround: Integer = 25; yround: Integer = 25); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawRoundRect3';
procedure QPainter_drawTiledPixmap(handle: QPainterH; rect: QRectFH; pm: QPixmapH; offset: QPointFH = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawTiledPixmap';
procedure QPainter_drawTiledPixmap(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; p5: QPixmapH; sx: Integer = 0; sy: Integer = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawTiledPixmap2';
procedure QPainter_drawTiledPixmap(handle: QPainterH; p1: PRect; p2: QPixmapH; p3: PPoint = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawTiledPixmap3';
procedure QPainter_drawPicture(handle: QPainterH; p: QPointFH; picture: QPictureH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPicture';
procedure QPainter_drawPicture(handle: QPainterH; x: Integer; y: Integer; picture: QPictureH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPicture2';
procedure QPainter_drawPicture(handle: QPainterH; p: PPoint; picture: QPictureH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPicture3';
procedure QPainter_drawPixmap(handle: QPainterH; targetRect: QRectFH; pixmap: QPixmapH; sourceRect: QRectFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPixmap';
procedure QPainter_drawPixmap(handle: QPainterH; targetRect: PRect; pixmap: QPixmapH; sourceRect: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPixmap2';
procedure QPainter_drawPixmap(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; pm: QPixmapH; sx: Integer; sy: Integer; sw: Integer; sh: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPixmap3';
procedure QPainter_drawPixmap(handle: QPainterH; x: Integer; y: Integer; pm: QPixmapH; sx: Integer; sy: Integer; sw: Integer; sh: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPixmap4';
procedure QPainter_drawPixmap(handle: QPainterH; p: QPointFH; pm: QPixmapH; sr: QRectFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPixmap5';
procedure QPainter_drawPixmap(handle: QPainterH; p: PPoint; pm: QPixmapH; sr: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPixmap6';
procedure QPainter_drawPixmap(handle: QPainterH; p: QPointFH; pm: QPixmapH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPixmap7';
procedure QPainter_drawPixmap(handle: QPainterH; p: PPoint; pm: QPixmapH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPixmap8';
procedure QPainter_drawPixmap(handle: QPainterH; x: Integer; y: Integer; pm: QPixmapH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPixmap9';
procedure QPainter_drawPixmap(handle: QPainterH; r: PRect; pm: QPixmapH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPixmap10';
procedure QPainter_drawPixmap(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; pm: QPixmapH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawPixmap11';
procedure QPainter_drawImage(handle: QPainterH; targetRect: QRectFH; image: QImageH; sourceRect: QRectFH; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawImage';
procedure QPainter_drawImage(handle: QPainterH; targetRect: PRect; image: QImageH; sourceRect: PRect; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawImage2';
procedure QPainter_drawImage(handle: QPainterH; p: QPointFH; image: QImageH; sr: QRectFH; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawImage3';
procedure QPainter_drawImage(handle: QPainterH; p: PPoint; image: QImageH; sr: PRect; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawImage4';
procedure QPainter_drawImage(handle: QPainterH; r: QRectFH; image: QImageH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawImage5';
procedure QPainter_drawImage(handle: QPainterH; r: PRect; image: QImageH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawImage6';
procedure QPainter_drawImage(handle: QPainterH; p: QPointFH; image: QImageH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawImage7';
procedure QPainter_drawImage(handle: QPainterH; p: PPoint; image: QImageH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawImage8';
procedure QPainter_drawImage(handle: QPainterH; x: Integer; y: Integer; image: QImageH; sx: Integer = 0; sy: Integer = 0; sw: Integer = -1; sh: Integer = -1; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawImage9';
procedure QPainter_setLayoutDirection(handle: QPainterH; direction: QtLayoutDirection); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setLayoutDirection';
function QPainter_layoutDirection(handle: QPainterH): QtLayoutDirection; cdecl; external QtShareName name QtNamePrefix + 'QPainter_layoutDirection';
procedure QPainter_drawText(handle: QPainterH; p: QPointFH; s: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawText';
procedure QPainter_drawText(handle: QPainterH; p: PPoint; s: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawText2';
procedure QPainter_drawText(handle: QPainterH; x: Integer; y: Integer; s: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawText3';
procedure QPainter_drawText(handle: QPainterH; r: QRectFH; flags: Integer; text: PWideString; br: QRectFH = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawText4';
procedure QPainter_drawText(handle: QPainterH; r: PRect; flags: Integer; text: PWideString; br: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawText5';
procedure QPainter_drawText(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; flags: Integer; text: PWideString; br: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawText6';
procedure QPainter_drawText(handle: QPainterH; r: QRectFH; text: PWideString; o: QTextOptionH = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_drawText7';
procedure QPainter_boundingRect(handle: QPainterH; retval: QRectFH; rect: QRectFH; flags: Integer; text: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_boundingRect';
procedure QPainter_boundingRect(handle: QPainterH; retval: PRect; rect: PRect; flags: Integer; text: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_boundingRect2';
procedure QPainter_boundingRect(handle: QPainterH; retval: PRect; x: Integer; y: Integer; w: Integer; h: Integer; flags: Integer; text: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_boundingRect3';
procedure QPainter_boundingRect(handle: QPainterH; retval: QRectFH; rect: QRectFH; text: PWideString; o: QTextOptionH = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_boundingRect4';
procedure QPainter_fillRect(handle: QPainterH; p1: QRectFH; p2: QBrushH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_fillRect';
procedure QPainter_fillRect(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; p5: QBrushH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_fillRect2';
procedure QPainter_fillRect(handle: QPainterH; p1: PRect; p2: QBrushH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_fillRect3';
procedure QPainter_eraseRect(handle: QPainterH; p1: QRectFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_eraseRect';
procedure QPainter_eraseRect(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_eraseRect2';
procedure QPainter_eraseRect(handle: QPainterH; p1: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QPainter_eraseRect3';
procedure QPainter_setRenderHint(handle: QPainterH; hint: QPainterRenderHint; _on: Boolean = True); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setRenderHint';
function QPainter_renderHints(handle: QPainterH): QPainterRenderHints; cdecl; external QtShareName name QtNamePrefix + 'QPainter_renderHints';
function QPainter_paintEngine(handle: QPainterH): QPaintEngineH; cdecl; external QtShareName name QtNamePrefix + 'QPainter_paintEngine';
procedure QPainter_setRedirected(device: QPaintDeviceH; replacement: QPaintDeviceH; offset: PPoint = nil); cdecl; external QtShareName name QtNamePrefix + 'QPainter_setRedirected';
function QPainter_redirected(device: QPaintDeviceH; offset: PPoint = 0): QPaintDeviceH; cdecl; external QtShareName name QtNamePrefix + 'QPainter_redirected';
procedure QPainter_restoreRedirected(device: QPaintDeviceH); cdecl; external QtShareName name QtNamePrefix + 'QPainter_restoreRedirected';


type
  QPaintEnginePolygonDrawMode = ( // QPaintEngine::PolygonDrawMode (1)
    QPaintEngineOddEvenMode, QPaintEngineWindingMode, QPaintEngineConvexMode, QPaintEnginePolylineMode );

type
  QPaintEnginePaintEngineFeature = cardinal; // QPaintEngine::PaintEngineFeature
  QPaintEnginePaintEngineFeatures = QPaintEnginePaintEngineFeature; //QFlags<> (3)
const
  QPaintEnginePrimitiveTransform =   $00000001;
  QPaintEnginePatternTransform =   $00000002;
  QPaintEnginePixmapTransform =   $00000004;
  QPaintEnginePatternBrush =   $00000008;
  QPaintEngineLinearGradientFill =   $00000010;
  QPaintEngineRadialGradientFill =   $00000020;
  QPaintEngineConicalGradientFill =   $00000040;
  QPaintEngineAlphaBlend =   $00000080;
  QPaintEnginePorterDuff =   $00000100;
  QPaintEnginePainterPaths =   $00000200;
  QPaintEngineAntialiasing =   $00000400;
  QPaintEngineBrushStroke =   $00000800;
  QPaintEnginePaintOutsidePaintEvent =   $20000000;
  QPaintEngineAllFeatures =   $ffffffff;

type
  QPaintEngineDirtyFlag = cardinal; // QPaintEngine::DirtyFlag
  QPaintEngineDirtyFlags = QPaintEngineDirtyFlag; //QFlags<> (3)
const
  QPaintEngineDirtyPen =   $0001;
  QPaintEngineDirtyBrush =   $0002;
  QPaintEngineDirtyBrushOrigin =   $0004;
  QPaintEngineDirtyFont =   $0008;
  QPaintEngineDirtyBackground =   $0010;
  QPaintEngineDirtyBackgroundMode =   $0020;
  QPaintEngineDirtyTransform =   $0040;
  QPaintEngineDirtyClipRegion =   $0080;
  QPaintEngineDirtyClipPath =   $0100;
  QPaintEngineDirtyHints =   $0200;
  QPaintEngineDirtyCompositionMode =   $0400;
  QPaintEngineDirtyClipEnabled =   $0800;
  QPaintEngineAllDirty =   $ffff;


type
  QPaintEngineType = (  //QPaintEngine::Type (2)
    QPaintEngineX11,
    QPaintEngineWindows,
    QPaintEngineQuickDraw,
    QPaintEngineCoreGraphics,
    QPaintEngineMacPrinter,
    QPaintEngineQWindowSystem,
    QPaintEnginePostScript,
    QPaintEngineOpenGL,
    QPaintEnginePicture,
    QPaintEngineSVG,
    QPaintEngineRaster,
    QPaintEngineUser = 50,
    QPaintEngineMaxUser = 100 );

function QPaintEngine_isActive(handle: QPaintEngineH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_isActive';
procedure QPaintEngine_setActive(handle: QPaintEngineH; newState: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_setActive';
function QPaintEngine_begin(handle: QPaintEngineH; pdev: QPaintDeviceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_begin';
function QPaintEngine_end(handle: QPaintEngineH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_end';
procedure QPaintEngine_drawRects(handle: QPaintEngineH; rects: PRect; rectCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawRects';
procedure QPaintEngine_drawRects(handle: QPaintEngineH; rects: QRectFH; rectCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawRects2';
procedure QPaintEngine_drawLines(handle: QPaintEngineH; lines: QLineH; lineCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawLines';
procedure QPaintEngine_drawLines(handle: QPaintEngineH; lines: QLineFH; lineCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawLines2';
procedure QPaintEngine_drawEllipse(handle: QPaintEngineH; r: QRectFH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawEllipse';
procedure QPaintEngine_drawEllipse(handle: QPaintEngineH; r: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawEllipse2';
procedure QPaintEngine_drawPath(handle: QPaintEngineH; path: QPainterPathH); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawPath';
procedure QPaintEngine_drawPoints(handle: QPaintEngineH; points: QPointFH; pointCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawPoints';
procedure QPaintEngine_drawPoints(handle: QPaintEngineH; points: PPoint; pointCount: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawPoints2';
procedure QPaintEngine_drawPolygon(handle: QPaintEngineH; points: QPointFH; pointCount: Integer; mode: QPaintEnginePolygonDrawMode); overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawPolygon';
procedure QPaintEngine_drawPolygon(handle: QPaintEngineH; points: PPoint; pointCount: Integer; mode: QPaintEnginePolygonDrawMode); overload; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawPolygon2';
procedure QPaintEngine_drawPixmap(handle: QPaintEngineH; r: QRectFH; pm: QPixmapH; sr: QRectFH); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawPixmap';
procedure QPaintEngine_drawTiledPixmap(handle: QPaintEngineH; r: QRectFH; pixmap: QPixmapH; s: QPointFH); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawTiledPixmap';
procedure QPaintEngine_drawImage(handle: QPaintEngineH; r: QRectFH; pm: QImageH; sr: QRectFH; flags: QtImageConversionFlags = QtAutoColor); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_drawImage';
procedure QPaintEngine_setPaintDevice(handle: QPaintEngineH; device: QPaintDeviceH); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_setPaintDevice';
function QPaintEngine_paintDevice(handle: QPaintEngineH): QPaintDeviceH; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_paintDevice';
procedure QPaintEngine_setSystemClip(handle: QPaintEngineH; baseClip: QRegionH); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_setSystemClip';
procedure QPaintEngine_systemClip(handle: QPaintEngineH; retval: QRegionH); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_systemClip';
procedure QPaintEngine_setSystemRect(handle: QPaintEngineH; rect: PRect); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_setSystemRect';
procedure QPaintEngine_systemRect(handle: QPaintEngineH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_systemRect';
function QPaintEngine_getDC(handle: QPaintEngineH): HDC; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_getDC';
procedure QPaintEngine_releaseDC(handle: QPaintEngineH; hdc: HDC); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_releaseDC';
procedure QPaintEngine_coordinateOffset(handle: QPaintEngineH; retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_coordinateOffset';
function QPaintEngine_type(handle: QPaintEngineH): QPaintEngineType; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_type';
procedure QPaintEngine_fix_neg_rect(handle: QPaintEngineH; x: PInteger; y: PInteger; w: PInteger; h: PInteger); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_fix_neg_rect';
function QPaintEngine_testDirty(handle: QPaintEngineH; df: QPaintEngineDirtyFlags): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_testDirty';
procedure QPaintEngine_setDirty(handle: QPaintEngineH; df: QPaintEngineDirtyFlags); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_setDirty';
procedure QPaintEngine_clearDirty(handle: QPaintEngineH; df: QPaintEngineDirtyFlags); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_clearDirty';
function QPaintEngine_hasFeature(handle: QPaintEngineH; feature: QPaintEnginePaintEngineFeatures): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_hasFeature';
function QPaintEngine_painter(handle: QPaintEngineH): QPainterH; cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_painter';
procedure QPaintEngine_syncState(handle: QPaintEngineH); cdecl; external QtShareName name QtNamePrefix + 'QPaintEngine_syncState';


type
  QPaintDevicePaintDeviceMetric = (  //QPaintDevice::PaintDeviceMetric (2)
    QPaintDevicePdmWidth = 1,
    QPaintDevicePdmHeight,
    QPaintDevicePdmWidthMM,
    QPaintDevicePdmHeightMM,
    QPaintDevicePdmNumColors,
    QPaintDevicePdmDepth,
    QPaintDevicePdmDpiX,
    QPaintDevicePdmDpiY,
    QPaintDevicePdmPhysicalDpiX,
    QPaintDevicePdmPhysicalDpiY );

function QPaintDevice_devType(handle: QPaintDeviceH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_devType';
function QPaintDevice_paintingActive(handle: QPaintDeviceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_paintingActive';
function QPaintDevice_paintEngine(handle: QPaintDeviceH): QPaintEngineH; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_paintEngine';
function QPaintDevice_getDC(handle: QPaintDeviceH): HDC; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_getDC';
procedure QPaintDevice_releaseDC(handle: QPaintDeviceH; hdc: HDC); cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_releaseDC';
function QPaintDevice_width(handle: QPaintDeviceH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_width';
function QPaintDevice_height(handle: QPaintDeviceH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_height';
function QPaintDevice_widthMM(handle: QPaintDeviceH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_widthMM';
function QPaintDevice_heightMM(handle: QPaintDeviceH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_heightMM';
function QPaintDevice_logicalDpiX(handle: QPaintDeviceH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_logicalDpiX';
function QPaintDevice_logicalDpiY(handle: QPaintDeviceH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_logicalDpiY';
function QPaintDevice_physicalDpiX(handle: QPaintDeviceH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_physicalDpiX';
function QPaintDevice_physicalDpiY(handle: QPaintDeviceH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_physicalDpiY';
function QPaintDevice_numColors(handle: QPaintDeviceH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_numColors';
function QPaintDevice_depth(handle: QPaintDeviceH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPaintDevice_depth';


type
  QRegionRegionType = ( // QRegion::RegionType (1)
    QRegionRectangle, QRegionEllipse );

function QRegion_create(): QRegionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_create';
procedure QRegion_destroy(handle: QRegionH); cdecl; external QtShareName name QtNamePrefix + 'QRegion_destroy'; 
function QRegion_create(x: Integer; y: Integer; w: Integer; h: Integer; t: QRegionRegionType = QRegionRectangle): QRegionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_create2';
function QRegion_create(r: PRect; t: QRegionRegionType = QRegionRectangle): QRegionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_create3';
function QRegion_create(pa: QPolygonH; fillRule: QtFillRule = QtOddEvenFill): QRegionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_create4';
function QRegion_create(region: QRegionH): QRegionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_create5';
function QRegion_create(bitmap: QBitmapH): QRegionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_create6';
function QRegion_isEmpty(handle: QRegionH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QRegion_isEmpty';
function QRegion_contains(handle: QRegionH; p: PPoint): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_contains';
function QRegion_contains(handle: QRegionH; r: PRect): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_contains2';
procedure QRegion_translate(handle: QRegionH; dx: Integer; dy: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_translate';
procedure QRegion_translate(handle: QRegionH; p: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_translate2';
procedure QRegion_translated(handle: QRegionH; retval: QRegionH; dx: Integer; dy: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_translated';
procedure QRegion_translated(handle: QRegionH; retval: QRegionH; p: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QRegion_translated2';
procedure QRegion_unite(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtShareName name QtNamePrefix + 'QRegion_unite';
procedure QRegion_intersect(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtShareName name QtNamePrefix + 'QRegion_intersect';
procedure QRegion_subtract(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtShareName name QtNamePrefix + 'QRegion_subtract';
procedure QRegion_eor(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtShareName name QtNamePrefix + 'QRegion_eor';
procedure QRegion_boundingRect(handle: QRegionH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QRegion_boundingRect';
procedure QRegion_setRects(handle: QRegionH; rect: PRect; num: Integer); cdecl; external QtShareName name QtNamePrefix + 'QRegion_setRects';
function QRegion_handle(handle: QRegionH): HRGN; cdecl; external QtShareName name QtNamePrefix + 'QRegion_handle';


type
  QPrinterPrinterMode = ( // QPrinter::PrinterMode (1)
    QPrinterScreenResolution, QPrinterPrinterResolution, QPrinterHighResolution );

  QPrinterOrientation = ( // QPrinter::Orientation (1)
    QPrinterPortrait, QPrinterLandscape );

  QPrinterPageOrder = ( // QPrinter::PageOrder (1)
    QPrinterFirstPageFirst, QPrinterLastPageFirst );

  QPrinterColorMode = ( // QPrinter::ColorMode (1)
    QPrinterGrayScale, QPrinterColor );

  QPrinterPaperSource = ( // QPrinter::PaperSource (1)
    QPrinterOnlyOne, QPrinterLower, QPrinterMiddle, QPrinterManual, QPrinterEnvelope, QPrinterEnvelopeManual, QPrinterAuto, QPrinterTractor, QPrinterSmallFormat, QPrinterLargeFormat, QPrinterLargeCapacity, 
    QPrinterCassette, QPrinterFormSource );

  QPrinterPrinterState = ( // QPrinter::PrinterState (1)
    QPrinterIdle, QPrinterActive, QPrinterAborted, QPrinterError );

  QPrinterOutputFormat = ( // QPrinter::OutputFormat (1)
    QPrinterNativeFormat, QPrinterPdfFormat );

  QPrinterPrintRange = ( // QPrinter::PrintRange (1)
    QPrinterAllPages, QPrinterSelection, QPrinterPageRange );

type
  QPrinterPageSize = cardinal; //  QPrinter::PageSize (4)

const
    QPrinterA4 = 0 { $0 };
    QPrinterB5 = 1 { $1 };
    QPrinterLetter = 2 { $2 };
    QPrinterLegal = 3 { $3 };
    QPrinterExecutive = 4 { $4 };
    QPrinterA0 = 5 { $5 };
    QPrinterA1 = 6 { $6 };
    QPrinterA2 = 7 { $7 };
    QPrinterA3 = 8 { $8 };
    QPrinterA5 = 9 { $9 };
    QPrinterA6 = 10 { $a };
    QPrinterA7 = 11 { $b };
    QPrinterA8 = 12 { $c };
    QPrinterA9 = 13 { $d };
    QPrinterB0 = 14 { $e };
    QPrinterB1 = 15 { $f };
    QPrinterB10 = 16 { $10 };
    QPrinterB2 = 17 { $11 };
    QPrinterB3 = 18 { $12 };
    QPrinterB4 = 19 { $13 };
    QPrinterB6 = 20 { $14 };
    QPrinterB7 = 21 { $15 };
    QPrinterB8 = 22 { $16 };
    QPrinterB9 = 23 { $17 };
    QPrinterC5E = 24 { $18 };
    QPrinterComm10E = 25 { $19 };
    QPrinterDLE = 26 { $1a };
    QPrinterFolio = 27 { $1b };
    QPrinterLedger = 28 { $1c };
    QPrinterTabloid = 29 { $1d };
    QPrinterCustom = 30 { $1e };
    QPrinterNPageSize = 30 { $1e };


function QPrinter_create(mode: QPrinterPrinterMode = QPrinterScreenResolution): QPrinterH; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_create';
procedure QPrinter_destroy(handle: QPrinterH); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_destroy'; 
function QPrinter_devType(handle: QPrinterH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_devType';
procedure QPrinter_setOutputFormat(handle: QPrinterH; format: QPrinterOutputFormat); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setOutputFormat';
function QPrinter_outputFormat(handle: QPrinterH): QPrinterOutputFormat; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_outputFormat';
procedure QPrinter_setPrinterName(handle: QPrinterH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setPrinterName';
procedure QPrinter_printerName(handle: QPrinterH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_printerName';
procedure QPrinter_setOutputFileName(handle: QPrinterH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setOutputFileName';
procedure QPrinter_outputFileName(handle: QPrinterH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_outputFileName';
procedure QPrinter_setPrintProgram(handle: QPrinterH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setPrintProgram';
procedure QPrinter_printProgram(handle: QPrinterH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_printProgram';
procedure QPrinter_setDocName(handle: QPrinterH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setDocName';
procedure QPrinter_docName(handle: QPrinterH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_docName';
procedure QPrinter_setCreator(handle: QPrinterH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setCreator';
procedure QPrinter_creator(handle: QPrinterH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_creator';
procedure QPrinter_setOrientation(handle: QPrinterH; p1: QPrinterOrientation); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setOrientation';
function QPrinter_orientation(handle: QPrinterH): QPrinterOrientation; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_orientation';
procedure QPrinter_setPageSize(handle: QPrinterH; p1: QPrinterPageSize); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setPageSize';
function QPrinter_pageSize(handle: QPrinterH): QPrinterPageSize; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_pageSize';
procedure QPrinter_setPageOrder(handle: QPrinterH; p1: QPrinterPageOrder); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setPageOrder';
function QPrinter_pageOrder(handle: QPrinterH): QPrinterPageOrder; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_pageOrder';
procedure QPrinter_setResolution(handle: QPrinterH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setResolution';
function QPrinter_resolution(handle: QPrinterH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_resolution';
procedure QPrinter_setColorMode(handle: QPrinterH; p1: QPrinterColorMode); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setColorMode';
function QPrinter_colorMode(handle: QPrinterH): QPrinterColorMode; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_colorMode';
procedure QPrinter_setCollateCopies(handle: QPrinterH; collate: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setCollateCopies';
function QPrinter_collateCopies(handle: QPrinterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_collateCopies';
procedure QPrinter_setFullPage(handle: QPrinterH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setFullPage';
function QPrinter_fullPage(handle: QPrinterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_fullPage';
procedure QPrinter_setNumCopies(handle: QPrinterH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setNumCopies';
function QPrinter_numCopies(handle: QPrinterH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_numCopies';
procedure QPrinter_setPaperSource(handle: QPrinterH; p1: QPrinterPaperSource); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setPaperSource';
function QPrinter_paperSource(handle: QPrinterH): QPrinterPaperSource; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_paperSource';
procedure QPrinter_supportedResolutions(handle: QPrinterH; retval: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_supportedResolutions';
procedure QPrinter_setFontEmbeddingEnabled(handle: QPrinterH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setFontEmbeddingEnabled';
function QPrinter_fontEmbeddingEnabled(handle: QPrinterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_fontEmbeddingEnabled';
procedure QPrinter_setWinPageSize(handle: QPrinterH; winPageSize: Integer); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setWinPageSize';
function QPrinter_winPageSize(handle: QPrinterH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_winPageSize';
procedure QPrinter_paperRect(handle: QPrinterH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_paperRect';
procedure QPrinter_pageRect(handle: QPrinterH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_pageRect';
function QPrinter_newPage(handle: QPrinterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_newPage';
function QPrinter_abort(handle: QPrinterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_abort';
function QPrinter_printerState(handle: QPrinterH): QPrinterPrinterState; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_printerState';
function QPrinter_paintEngine(handle: QPrinterH): QPaintEngineH; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_paintEngine';
function QPrinter_printEngine(handle: QPrinterH): QPrintEngineH; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_printEngine';
function QPrinter_getDC(handle: QPrinterH): HDC; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_getDC';
procedure QPrinter_releaseDC(handle: QPrinterH; hdc: HDC); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_releaseDC';
procedure QPrinter_setFromTo(handle: QPrinterH; fromPage: Integer; toPage: Integer); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setFromTo';
function QPrinter_fromPage(handle: QPrinterH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_fromPage';
function QPrinter_toPage(handle: QPrinterH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_toPage';
procedure QPrinter_setPrintRange(handle: QPrinterH; range: QPrinterPrintRange); cdecl; external QtShareName name QtNamePrefix + 'QPrinter_setPrintRange';
function QPrinter_printRange(handle: QPrinterH): QPrinterPrintRange; cdecl; external QtShareName name QtNamePrefix + 'QPrinter_printRange';


type
  QFontStyle = ( // QFont::Style (1)
    QFontStyleNormal, QFontStyleItalic, QFontStyleOblique );

  QFontStyleStrategy = (  //QFont::StyleStrategy (2s)
    QFontPreferDefault = $0001,
    QFontPreferBitmap = $0002,
    QFontPreferDevice = $0004,
    QFontPreferOutline = $0008,
    QFontForceOutline = $0010,
    QFontPreferMatch = $0020,
    QFontPreferQuality = $0040,
    QFontPreferAntialias = $0080,
    QFontNoAntialias = $0100,
    QFontOpenGLCompatible = $0200 );

  QFontWeight = (  //QFont::Weight (2s)
    QFontLight = 25,
    QFontNormal = 50,
    QFontDemiBold = 63,
    QFontBold = 75,
    QFontBlack = 87 );

  QFontStretch = (  //QFont::Stretch (2s)
    QFontUltraCondensed = 50,
    QFontExtraCondensed = 62,
    QFontCondensed = 75,
    QFontSemiCondensed = 87,
    QFontUnstretched = 100,
    QFontSemiExpanded = 112,
    QFontExpanded = 125,
    QFontExtraExpanded = 150,
    QFontUltraExpanded = 200 );

type
  QFontStyleHint = cardinal; //  QFont::StyleHint (4)

const
    QFontHelvetica = 0 { $0 };
    QFontSansSerif = 0 { $0 };
    QFontTimes = 1 { $1 };
    QFontSerif = 1 { $1 };
    QFontCourier = 2 { $2 };
    QFontTypeWriter = 2 { $2 };
    QFontOldEnglish = 3 { $3 };
    QFontDecorative = 3 { $3 };
    QFontSystem = 4 { $4 };
    QFontAnyStyle = 5 { $5 };


function QFont_create(): QFontH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFont_create';
procedure QFont_destroy(handle: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QFont_destroy'; 
function QFont_create(family: PWideString; pointSize: Integer = -1; weight: Integer = -1; italic: Boolean = False): QFontH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFont_create2';
function QFont_create(p1: QFontH; pd: QPaintDeviceH): QFontH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFont_create3';
function QFont_create(p1: QFontH): QFontH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFont_create4';
procedure QFont_family(handle: QFontH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_family';
procedure QFont_setFamily(handle: QFontH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_setFamily';
function QFont_pointSize(handle: QFontH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFont_pointSize';
procedure QFont_setPointSize(handle: QFontH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QFont_setPointSize';
function QFont_pointSizeF(handle: QFontH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFont_pointSizeF';
procedure QFont_setPointSizeF(handle: QFontH; p1: Double); cdecl; external QtShareName name QtNamePrefix + 'QFont_setPointSizeF';
function QFont_pixelSize(handle: QFontH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFont_pixelSize';
procedure QFont_setPixelSize(handle: QFontH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QFont_setPixelSize';
function QFont_weight(handle: QFontH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFont_weight';
procedure QFont_setWeight(handle: QFontH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QFont_setWeight';
function QFont_bold(handle: QFontH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFont_bold';
procedure QFont_setBold(handle: QFontH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QFont_setBold';
procedure QFont_setStyle(handle: QFontH; style: QFontStyle); cdecl; external QtShareName name QtNamePrefix + 'QFont_setStyle';
function QFont_style(handle: QFontH): QFontStyle; cdecl; external QtShareName name QtNamePrefix + 'QFont_style';
function QFont_italic(handle: QFontH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFont_italic';
procedure QFont_setItalic(handle: QFontH; b: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QFont_setItalic';
function QFont_underline(handle: QFontH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFont_underline';
procedure QFont_setUnderline(handle: QFontH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QFont_setUnderline';
function QFont_overline(handle: QFontH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFont_overline';
procedure QFont_setOverline(handle: QFontH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QFont_setOverline';
function QFont_strikeOut(handle: QFontH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFont_strikeOut';
procedure QFont_setStrikeOut(handle: QFontH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QFont_setStrikeOut';
function QFont_fixedPitch(handle: QFontH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFont_fixedPitch';
procedure QFont_setFixedPitch(handle: QFontH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QFont_setFixedPitch';
function QFont_kerning(handle: QFontH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFont_kerning';
procedure QFont_setKerning(handle: QFontH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QFont_setKerning';
function QFont_styleHint(handle: QFontH): QFontStyleHint; cdecl; external QtShareName name QtNamePrefix + 'QFont_styleHint';
function QFont_styleStrategy(handle: QFontH): QFontStyleStrategy; cdecl; external QtShareName name QtNamePrefix + 'QFont_styleStrategy';
procedure QFont_setStyleHint(handle: QFontH; p1: QFontStyleHint; p2: QFontStyleStrategy = QFontPreferDefault); cdecl; external QtShareName name QtNamePrefix + 'QFont_setStyleHint';
procedure QFont_setStyleStrategy(handle: QFontH; s: QFontStyleStrategy); cdecl; external QtShareName name QtNamePrefix + 'QFont_setStyleStrategy';
function QFont_stretch(handle: QFontH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFont_stretch';
procedure QFont_setStretch(handle: QFontH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QFont_setStretch';
function QFont_rawMode(handle: QFontH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFont_rawMode';
procedure QFont_setRawMode(handle: QFontH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QFont_setRawMode';
function QFont_exactMatch(handle: QFontH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFont_exactMatch';
function QFont_isCopyOf(handle: QFontH; p1: QFontH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFont_isCopyOf';
function QFont_handle(handle: QFontH): HFONT; cdecl; external QtShareName name QtNamePrefix + 'QFont_handle';
procedure QFont_setRawName(handle: QFontH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_setRawName';
procedure QFont_rawName(handle: QFontH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_rawName';
procedure QFont_key(handle: QFontH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_key';
procedure QFont_toString(handle: QFontH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_toString';
function QFont_fromString(handle: QFontH; p1: PWideString): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFont_fromString';
procedure QFont_substitute(retval: PWideString; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_substitute';
procedure QFont_substitutes(retval: QStringListH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_substitutes';
procedure QFont_substitutions(retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QFont_substitutions';
procedure QFont_insertSubstitution(p1: PWideString; p2: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_insertSubstitution';
procedure QFont_insertSubstitutions(p1: PWideString; p2: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QFont_insertSubstitutions';
procedure QFont_removeSubstitution(p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_removeSubstitution';
procedure QFont_initialize(); cdecl; external QtShareName name QtNamePrefix + 'QFont_initialize';
procedure QFont_cleanup(); cdecl; external QtShareName name QtNamePrefix + 'QFont_cleanup';
procedure QFont_cacheStatistics(); cdecl; external QtShareName name QtNamePrefix + 'QFont_cacheStatistics';
procedure QFont_defaultFamily(handle: QFontH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_defaultFamily';
procedure QFont_lastResortFamily(handle: QFontH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_lastResortFamily';
procedure QFont_lastResortFont(handle: QFontH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFont_lastResortFont';
procedure QFont_resolve(handle: QFontH; retval: QFontH; p1: QFontH); overload; cdecl; external QtShareName name QtNamePrefix + 'QFont_resolve';
function QFont_resolve(handle: QFontH): Cardinal; overload; cdecl; external QtShareName name QtNamePrefix + 'QFont_resolve2';
procedure QFont_resolve(handle: QFontH; mask: Cardinal); overload; cdecl; external QtShareName name QtNamePrefix + 'QFont_resolve3';


type
  QFontDatabaseWritingSystem = ( // QFontDatabase::WritingSystem (1)
    QFontDatabaseAny, QFontDatabaseLatin, QFontDatabaseGreek, QFontDatabaseCyrillic, QFontDatabaseArmenian, QFontDatabaseHebrew, QFontDatabaseArabic, QFontDatabaseSyriac, QFontDatabaseThaana, 
    QFontDatabaseDevanagari, QFontDatabaseBengali, QFontDatabaseGurmukhi, QFontDatabaseGujarati, QFontDatabaseOriya, QFontDatabaseTamil, QFontDatabaseTelugu, QFontDatabaseKannada, QFontDatabaseMalayalam, 
    QFontDatabaseSinhala, QFontDatabaseThai, QFontDatabaseLao, QFontDatabaseTibetan, QFontDatabaseMyanmar, QFontDatabaseGeorgian, QFontDatabaseKhmer, QFontDatabaseSimplifiedChinese, QFontDatabaseTraditionalChinese, 
    QFontDatabaseJapanese, QFontDatabaseKorean, QFontDatabaseVietnamese, QFontDatabaseOther, QFontDatabaseWritingSystemsCount );

procedure QFontDatabase_standardSizes(retval: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_standardSizes';
function QFontDatabase_create(): QFontDatabaseH; cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_create';
procedure QFontDatabase_destroy(handle: QFontDatabaseH); cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_destroy'; 
procedure QFontDatabase_writingSystems(handle: QFontDatabaseH; retval: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_writingSystems';
procedure QFontDatabase_families(handle: QFontDatabaseH; retval: QStringListH; writingSystem: QFontDatabaseWritingSystem = QFontDatabaseAny); cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_families';
procedure QFontDatabase_styles(handle: QFontDatabaseH; retval: QStringListH; family: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_styles';
procedure QFontDatabase_pointSizes(handle: QFontDatabaseH; retval: PIntArray; family: PWideString; style: PWideString = nil); cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_pointSizes';
procedure QFontDatabase_smoothSizes(handle: QFontDatabaseH; retval: PIntArray; family: PWideString; style: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_smoothSizes';
procedure QFontDatabase_styleString(handle: QFontDatabaseH; retval: PWideString; font: QFontH); overload; cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_styleString';
procedure QFontDatabase_styleString(handle: QFontDatabaseH; retval: PWideString; fontInfo: QFontInfoH); overload; cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_styleString2';
procedure QFontDatabase_font(handle: QFontDatabaseH; retval: QFontH; family: PWideString; style: PWideString; pointSize: Integer); cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_font';
function QFontDatabase_isBitmapScalable(handle: QFontDatabaseH; family: PWideString; style: PWideString = nil): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_isBitmapScalable';
function QFontDatabase_isSmoothlyScalable(handle: QFontDatabaseH; family: PWideString; style: PWideString = nil): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_isSmoothlyScalable';
function QFontDatabase_isScalable(handle: QFontDatabaseH; family: PWideString; style: PWideString = nil): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_isScalable';
function QFontDatabase_isFixedPitch(handle: QFontDatabaseH; family: PWideString; style: PWideString = nil): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_isFixedPitch';
function QFontDatabase_italic(handle: QFontDatabaseH; family: PWideString; style: PWideString): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_italic';
function QFontDatabase_bold(handle: QFontDatabaseH; family: PWideString; style: PWideString): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_bold';
function QFontDatabase_weight(handle: QFontDatabaseH; family: PWideString; style: PWideString): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_weight';
procedure QFontDatabase_writingSystemName(retval: PWideString; writingSystem: QFontDatabaseWritingSystem); cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_writingSystemName';
procedure QFontDatabase_writingSystemSample(retval: PWideString; writingSystem: QFontDatabaseWritingSystem); cdecl; external QtShareName name QtNamePrefix + 'QFontDatabase_writingSystemSample';


type
  QTextOptionWrapMode = ( // QTextOption::WrapMode (1)
    QTextOptionNoWrap, QTextOptionWordWrap, QTextOptionManualWrap, QTextOptionWrapAnywhere, QTextOptionWrapAtWordBoundaryOrAnywhere );

type
  QTextOptionFlag = cardinal; // QTextOption::Flag
  QTextOptionFlags = QTextOptionFlag; //QFlags<> (3)
const
  QTextOptionIncludeTrailingSpaces =   $80000000;

function QTextOption_create(): QTextOptionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTextOption_create';
procedure QTextOption_destroy(handle: QTextOptionH); cdecl; external QtShareName name QtNamePrefix + 'QTextOption_destroy'; 
function QTextOption_create(alignment: QtAlignment): QTextOptionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTextOption_create2';
function QTextOption_create(o: QTextOptionH): QTextOptionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTextOption_create3';
procedure QTextOption_setAlignment(handle: QTextOptionH; alignment: QtAlignment); cdecl; external QtShareName name QtNamePrefix + 'QTextOption_setAlignment';
function QTextOption_alignment(handle: QTextOptionH): QtAlignment; cdecl; external QtShareName name QtNamePrefix + 'QTextOption_alignment';
procedure QTextOption_setTextDirection(handle: QTextOptionH; aDirection: QtLayoutDirection); cdecl; external QtShareName name QtNamePrefix + 'QTextOption_setTextDirection';
function QTextOption_textDirection(handle: QTextOptionH): QtLayoutDirection; cdecl; external QtShareName name QtNamePrefix + 'QTextOption_textDirection';
procedure QTextOption_setWrapMode(handle: QTextOptionH; wrap: QTextOptionWrapMode); cdecl; external QtShareName name QtNamePrefix + 'QTextOption_setWrapMode';
function QTextOption_wrapMode(handle: QTextOptionH): QTextOptionWrapMode; cdecl; external QtShareName name QtNamePrefix + 'QTextOption_wrapMode';
procedure QTextOption_setFlags(handle: QTextOptionH; flags: QTextOptionFlags); cdecl; external QtShareName name QtNamePrefix + 'QTextOption_setFlags';
function QTextOption_flags(handle: QTextOptionH): QTextOptionFlags; cdecl; external QtShareName name QtNamePrefix + 'QTextOption_flags';
procedure QTextOption_setTabStop(handle: QTextOptionH; tabStop: Double); cdecl; external QtShareName name QtNamePrefix + 'QTextOption_setTabStop';
function QTextOption_tabStop(handle: QTextOptionH): Double; cdecl; external QtShareName name QtNamePrefix + 'QTextOption_tabStop';
procedure QTextOption_setTabArray(handle: QTextOptionH; tabStops: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QTextOption_setTabArray';
procedure QTextOption_tabArray(handle: QTextOptionH; retval: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QTextOption_tabArray';
procedure QTextOption_setUseDesignMetrics(handle: QTextOptionH; b: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTextOption_setUseDesignMetrics';
function QTextOption_useDesignMetrics(handle: QTextOptionH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextOption_useDesignMetrics';

function QFontMetrics_create(p1: QFontH): QFontMetricsH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_create';
procedure QFontMetrics_destroy(handle: QFontMetricsH); cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_destroy'; 
function QFontMetrics_create(p1: QFontH; pd: QPaintDeviceH): QFontMetricsH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_create2';
function QFontMetrics_create(p1: QFontMetricsH): QFontMetricsH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_create3';
function QFontMetrics_ascent(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_ascent';
function QFontMetrics_descent(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_descent';
function QFontMetrics_height(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_height';
function QFontMetrics_leading(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_leading';
function QFontMetrics_lineSpacing(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_lineSpacing';
function QFontMetrics_minLeftBearing(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_minLeftBearing';
function QFontMetrics_minRightBearing(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_minRightBearing';
function QFontMetrics_maxWidth(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_maxWidth';
function QFontMetrics_xHeight(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_xHeight';
function QFontMetrics_inFont(handle: QFontMetricsH; p1: PWideChar): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_inFont';
function QFontMetrics_leftBearing(handle: QFontMetricsH; p1: PWideChar): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_leftBearing';
function QFontMetrics_rightBearing(handle: QFontMetricsH; p1: PWideChar): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_rightBearing';
function QFontMetrics_width(handle: QFontMetricsH; p1: PWideString; len: Integer = -1): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_width';
function QFontMetrics_width(handle: QFontMetricsH; p1: PWideChar): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_width2';
function QFontMetrics_charWidth(handle: QFontMetricsH; str: PWideString; pos: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_charWidth';
procedure QFontMetrics_boundingRect(handle: QFontMetricsH; retval: PRect; p1: PWideChar); overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_boundingRect';
procedure QFontMetrics_boundingRect(handle: QFontMetricsH; retval: PRect; text: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_boundingRect2';
procedure QFontMetrics_boundingRect(handle: QFontMetricsH; retval: PRect; r: PRect; flags: Integer; text: PWideString; tabstops: Integer = 0; tabarray: PInteger = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_boundingRect3';
procedure QFontMetrics_boundingRect(handle: QFontMetricsH; retval: PRect; x: Integer; y: Integer; w: Integer; h: Integer; flags: Integer; text: PWideString; tabstops: Integer = 0; tabarray: PInteger = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_boundingRect4';
procedure QFontMetrics_size(handle: QFontMetricsH; retval: PSize; flags: Integer; str: PWideString; tabstops: Integer = 0; tabarray: PInteger = 0); cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_size';
function QFontMetrics_underlinePos(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_underlinePos';
function QFontMetrics_overlinePos(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_overlinePos';
function QFontMetrics_strikeOutPos(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_strikeOutPos';
function QFontMetrics_lineWidth(handle: QFontMetricsH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontMetrics_lineWidth';

function QFontMetricsF_create(p1: QFontH): QFontMetricsFH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_create';
procedure QFontMetricsF_destroy(handle: QFontMetricsFH); cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_destroy'; 
function QFontMetricsF_create(p1: QFontH; pd: QPaintDeviceH): QFontMetricsFH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_create2';
function QFontMetricsF_ascent(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_ascent';
function QFontMetricsF_descent(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_descent';
function QFontMetricsF_height(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_height';
function QFontMetricsF_leading(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_leading';
function QFontMetricsF_lineSpacing(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_lineSpacing';
function QFontMetricsF_minLeftBearing(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_minLeftBearing';
function QFontMetricsF_minRightBearing(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_minRightBearing';
function QFontMetricsF_maxWidth(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_maxWidth';
function QFontMetricsF_xHeight(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_xHeight';
function QFontMetricsF_inFont(handle: QFontMetricsFH; p1: PWideChar): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_inFont';
function QFontMetricsF_leftBearing(handle: QFontMetricsFH; p1: PWideChar): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_leftBearing';
function QFontMetricsF_rightBearing(handle: QFontMetricsFH; p1: PWideChar): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_rightBearing';
function QFontMetricsF_width(handle: QFontMetricsFH; _string: PWideString): Double; overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_width';
function QFontMetricsF_width(handle: QFontMetricsFH; p1: PWideChar): Double; overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_width2';
procedure QFontMetricsF_boundingRect(handle: QFontMetricsFH; retval: QRectFH; _string: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_boundingRect';
procedure QFontMetricsF_boundingRect(handle: QFontMetricsFH; retval: QRectFH; p1: PWideChar); overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_boundingRect2';
procedure QFontMetricsF_boundingRect(handle: QFontMetricsFH; retval: QRectFH; r: QRectFH; flags: Integer; _string: PWideString; tabstops: Integer = 0; tabarray: PInteger = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_boundingRect3';
procedure QFontMetricsF_size(handle: QFontMetricsFH; retval: QSizeFH; flags: Integer; str: PWideString; tabstops: Integer = 0; tabarray: PInteger = 0); cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_size';
function QFontMetricsF_underlinePos(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_underlinePos';
function QFontMetricsF_overlinePos(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_overlinePos';
function QFontMetricsF_strikeOutPos(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_strikeOutPos';
function QFontMetricsF_lineWidth(handle: QFontMetricsFH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontMetricsF_lineWidth';

function QFontInfo_create(p1: QFontH): QFontInfoH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_create';
procedure QFontInfo_destroy(handle: QFontInfoH); cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_destroy'; 
function QFontInfo_create(p1: QFontInfoH): QFontInfoH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_create2';
procedure QFontInfo_family(handle: QFontInfoH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_family';
function QFontInfo_pixelSize(handle: QFontInfoH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_pixelSize';
function QFontInfo_pointSize(handle: QFontInfoH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_pointSize';
function QFontInfo_pointSizeF(handle: QFontInfoH): Double; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_pointSizeF';
function QFontInfo_italic(handle: QFontInfoH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_italic';
function QFontInfo_style(handle: QFontInfoH): QFontStyle; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_style';
function QFontInfo_weight(handle: QFontInfoH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_weight';
function QFontInfo_bold(handle: QFontInfoH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_bold';
function QFontInfo_underline(handle: QFontInfoH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_underline';
function QFontInfo_overline(handle: QFontInfoH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_overline';
function QFontInfo_strikeOut(handle: QFontInfoH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_strikeOut';
function QFontInfo_fixedPitch(handle: QFontInfoH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_fixedPitch';
function QFontInfo_styleHint(handle: QFontInfoH): QFontStyleHint; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_styleHint';
function QFontInfo_rawMode(handle: QFontInfoH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_rawMode';
function QFontInfo_exactMatch(handle: QFontInfoH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFontInfo_exactMatch';


type
  QTextDocumentMetaInformation = ( // QTextDocument::MetaInformation (1)
    QTextDocumentDocumentTitle );

type
  QTextDocumentFindFlag = cardinal; // QTextDocument::FindFlag
  QTextDocumentFindFlags = QTextDocumentFindFlag; //QFlags<> (3)
const
  QTextDocumentFindBackward =   $00001;
  QTextDocumentFindCaseSensitively =   $00002;
  QTextDocumentFindWholeWords =   $00004;


type
  QTextDocumentResourceType = (  //QTextDocument::ResourceType (2s)
    QTextDocumentHtmlResource = 1,
    QTextDocumentImageResource = 2,
    QTextDocumentUserResource = 100 );

function QTextDocument_create(parent: QObjectH = nil): QTextDocumentH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_create';
procedure QTextDocument_destroy(handle: QTextDocumentH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_destroy'; 
function QTextDocument_create(text: PWideString; parent: QObjectH = nil): QTextDocumentH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_create2';
function QTextDocument_clone(handle: QTextDocumentH; parent: QObjectH = nil): QTextDocumentH; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_clone';
function QTextDocument_isEmpty(handle: QTextDocumentH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_isEmpty';
procedure QTextDocument_clear(handle: QTextDocumentH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_clear';
procedure QTextDocument_setUndoRedoEnabled(handle: QTextDocumentH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_setUndoRedoEnabled';
function QTextDocument_isUndoRedoEnabled(handle: QTextDocumentH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_isUndoRedoEnabled';
function QTextDocument_isUndoAvailable(handle: QTextDocumentH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_isUndoAvailable';
function QTextDocument_isRedoAvailable(handle: QTextDocumentH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_isRedoAvailable';
procedure QTextDocument_setDocumentLayout(handle: QTextDocumentH; layout: QAbstractTextDocumentLayoutH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_setDocumentLayout';
function QTextDocument_documentLayout(handle: QTextDocumentH): QAbstractTextDocumentLayoutH; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_documentLayout';
procedure QTextDocument_setMetaInformation(handle: QTextDocumentH; info: QTextDocumentMetaInformation; p2: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_setMetaInformation';
procedure QTextDocument_metaInformation(handle: QTextDocumentH; retval: PWideString; info: QTextDocumentMetaInformation); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_metaInformation';
procedure QTextDocument_toHtml(handle: QTextDocumentH; retval: PWideString; encoding: QByteArrayH = nil); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_toHtml';
procedure QTextDocument_setHtml(handle: QTextDocumentH; html: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_setHtml';
procedure QTextDocument_toPlainText(handle: QTextDocumentH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_toPlainText';
procedure QTextDocument_setPlainText(handle: QTextDocumentH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_setPlainText';
procedure QTextDocument_find(handle: QTextDocumentH; retval: QTextCursorH; expr: PWideString; from: Integer = 0; options: QTextDocumentFindFlags = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_find';
procedure QTextDocument_find(handle: QTextDocumentH; retval: QTextCursorH; expr: PWideString; from: QTextCursorH; options: QTextDocumentFindFlags = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_find2';
function QTextDocument_frameAt(handle: QTextDocumentH; pos: Integer): QTextFrameH; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_frameAt';
function QTextDocument_rootFrame(handle: QTextDocumentH): QTextFrameH; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_rootFrame';
function QTextDocument_object(handle: QTextDocumentH; objectIndex: Integer): QTextObjectH; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_object';
function QTextDocument_objectForFormat(handle: QTextDocumentH; p1: QTextFormatH): QTextObjectH; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_objectForFormat';
procedure QTextDocument_findBlock(handle: QTextDocumentH; retval: QTextBlockH; pos: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_findBlock';
procedure QTextDocument_begin(handle: QTextDocumentH; retval: QTextBlockH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_begin';
procedure QTextDocument_end(handle: QTextDocumentH; retval: QTextBlockH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_end';
procedure QTextDocument_setPageSize(handle: QTextDocumentH; size: QSizeFH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_setPageSize';
procedure QTextDocument_pageSize(handle: QTextDocumentH; retval: QSizeFH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_pageSize';
procedure QTextDocument_setDefaultFont(handle: QTextDocumentH; font: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_setDefaultFont';
procedure QTextDocument_defaultFont(handle: QTextDocumentH; retval: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_defaultFont';
function QTextDocument_pageCount(handle: QTextDocumentH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_pageCount';
function QTextDocument_isModified(handle: QTextDocumentH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_isModified';
procedure QTextDocument_print(handle: QTextDocumentH; printer: QPrinterH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_print';
procedure QTextDocument_resource(handle: QTextDocumentH; retval: QVariantH; _type: Integer; name: QUrlH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_resource';
procedure QTextDocument_addResource(handle: QTextDocumentH; _type: Integer; name: QUrlH; resource: QVariantH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_addResource';
procedure QTextDocument_markContentsDirty(handle: QTextDocumentH; from: Integer; length: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_markContentsDirty';
procedure QTextDocument_setUseDesignMetrics(handle: QTextDocumentH; b: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_setUseDesignMetrics';
function QTextDocument_useDesignMetrics(handle: QTextDocumentH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_useDesignMetrics';
procedure QTextDocument_undo(handle: QTextDocumentH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_undo';
procedure QTextDocument_redo(handle: QTextDocumentH); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_redo';
procedure QTextDocument_setModified(handle: QTextDocumentH; m: Boolean = True); cdecl; external QtShareName name QtNamePrefix + 'QTextDocument_setModified';


type
  QIconMode = ( // QIcon::Mode (1)
    QIconNormal, QIconDisabled, QIconActive );

  QIconState = ( // QIcon::State (1)
    QIconOn, QIconOff );

function QIcon_create(): QIconH; overload; cdecl; external QtShareName name QtNamePrefix + 'QIcon_create';
procedure QIcon_destroy(handle: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QIcon_destroy'; 
function QIcon_create(pixmap: QPixmapH): QIconH; overload; cdecl; external QtShareName name QtNamePrefix + 'QIcon_create2';
function QIcon_create(other: QIconH): QIconH; overload; cdecl; external QtShareName name QtNamePrefix + 'QIcon_create3';
function QIcon_create(fileName: PWideString): QIconH; overload; cdecl; external QtShareName name QtNamePrefix + 'QIcon_create4';
function QIcon_create(engine: QIconEngineH): QIconH; overload; cdecl; external QtShareName name QtNamePrefix + 'QIcon_create5';
procedure QIcon_pixmap(handle: QIconH; retval: QPixmapH; size: PSize; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); overload; cdecl; external QtShareName name QtNamePrefix + 'QIcon_pixmap';
procedure QIcon_pixmap(handle: QIconH; retval: QPixmapH; w: Integer; h: Integer; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); overload; cdecl; external QtShareName name QtNamePrefix + 'QIcon_pixmap2';
procedure QIcon_pixmap(handle: QIconH; retval: QPixmapH; extent: Integer; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); overload; cdecl; external QtShareName name QtNamePrefix + 'QIcon_pixmap3';
procedure QIcon_actualSize(handle: QIconH; retval: PSize; size: PSize; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); cdecl; external QtShareName name QtNamePrefix + 'QIcon_actualSize';
procedure QIcon_paint(handle: QIconH; painter: QPainterH; rect: PRect; alignment: QtAlignment = QtAlignCenter; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); overload; cdecl; external QtShareName name QtNamePrefix + 'QIcon_paint';
procedure QIcon_paint(handle: QIconH; painter: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; alignment: QtAlignment = QtAlignCenter; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); overload; cdecl; external QtShareName name QtNamePrefix + 'QIcon_paint2';
function QIcon_isNull(handle: QIconH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIcon_isNull';
function QIcon_isDetached(handle: QIconH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIcon_isDetached';
function QIcon_serialNumber(handle: QIconH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QIcon_serialNumber';
procedure QIcon_addPixmap(handle: QIconH; pixmap: QPixmapH; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); cdecl; external QtShareName name QtNamePrefix + 'QIcon_addPixmap';
procedure QIcon_addFile(handle: QIconH; fileName: PWideString; size: PSize = nil; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); cdecl; external QtShareName name QtNamePrefix + 'QIcon_addFile';


type
  QPixmapHBitmapFormat = ( // QPixmap::HBitmapFormat (1)
    QPixmapNoAlpha, QPixmapPremultipliedAlpha );

function QPixmap_create(): QPixmapH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_create';
procedure QPixmap_destroy(handle: QPixmapH); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_destroy'; 
function QPixmap_create(w: Integer; h: Integer): QPixmapH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_create2';
function QPixmap_create(p1: PSize): QPixmapH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_create3';
function QPixmap_create(fileName: PWideString; format: PAnsiChar = 0; flags: QtImageConversionFlags = QtAutoColor): QPixmapH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_create4';
function QPixmap_create(xpm: PAnsiChar): QPixmapH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_create5';
function QPixmap_create(p1: QPixmapH): QPixmapH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_create6';
function QPixmap_isNull(handle: QPixmapH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_isNull';
function QPixmap_devType(handle: QPixmapH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_devType';
function QPixmap_width(handle: QPixmapH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_width';
function QPixmap_height(handle: QPixmapH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_height';
procedure QPixmap_size(handle: QPixmapH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_size';
procedure QPixmap_rect(handle: QPixmapH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_rect';
function QPixmap_depth(handle: QPixmapH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_depth';
function QPixmap_defaultDepth(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_defaultDepth';
procedure QPixmap_fill(handle: QPixmapH; fillColor: PQColor = Qtwhite); overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_fill';
procedure QPixmap_fill(handle: QPixmapH; widget: QWidgetH; ofs: PPoint); overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_fill2';
procedure QPixmap_fill(handle: QPixmapH; widget: QWidgetH; xofs: Integer; yofs: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_fill3';
procedure QPixmap_mask(handle: QPixmapH; retval: QBitmapH); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_mask';
procedure QPixmap_setMask(handle: QPixmapH; p1: QBitmapH); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_setMask';
procedure QPixmap_alphaChannel(handle: QPixmapH; retval: QPixmapH); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_alphaChannel';
procedure QPixmap_setAlphaChannel(handle: QPixmapH; p1: QPixmapH); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_setAlphaChannel';
function QPixmap_hasAlpha(handle: QPixmapH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_hasAlpha';
function QPixmap_hasAlphaChannel(handle: QPixmapH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_hasAlphaChannel';
procedure QPixmap_createHeuristicMask(handle: QPixmapH; retval: QBitmapH; clipTight: Boolean = True); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_createHeuristicMask';
procedure QPixmap_createMaskFromColor(handle: QPixmapH; retval: QBitmapH; maskColor: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_createMaskFromColor';
procedure QPixmap_grabWindow(retval: QPixmapH; p1: Cardinal; x: Integer = 0; y: Integer = 0; w: Integer = -1; h: Integer = -1); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_grabWindow';
procedure QPixmap_grabWidget(retval: QPixmapH; widget: QWidgetH; rect: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_grabWidget';
procedure QPixmap_grabWidget(retval: QPixmapH; widget: QWidgetH; x: Integer = 0; y: Integer = 0; w: Integer = -1; h: Integer = -1); overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_grabWidget2';
procedure QPixmap_scaled(handle: QPixmapH; retval: QPixmapH; w: Integer; h: Integer; aspectMode: QtAspectRatioMode = QtIgnoreAspectRatio; mode: QtTransformationMode = QtFastTransformation); overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_scaled';
procedure QPixmap_scaled(handle: QPixmapH; retval: QPixmapH; s: PSize; aspectMode: QtAspectRatioMode = QtIgnoreAspectRatio; mode: QtTransformationMode = QtFastTransformation); overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_scaled2';
procedure QPixmap_scaledToWidth(handle: QPixmapH; retval: QPixmapH; w: Integer; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_scaledToWidth';
procedure QPixmap_scaledToHeight(handle: QPixmapH; retval: QPixmapH; h: Integer; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_scaledToHeight';
procedure QPixmap_transformed(handle: QPixmapH; retval: QPixmapH; p1: QMatrixH; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_transformed';
procedure QPixmap_trueMatrix(retval: QMatrixH; m: QMatrixH; w: Integer; h: Integer); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_trueMatrix';
procedure QPixmap_toImage(handle: QPixmapH; retval: QImageH); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_toImage';
procedure QPixmap_fromImage(retval: QPixmapH; image: QImageH; flags: QtImageConversionFlags = QtAutoColor); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_fromImage';
function QPixmap_load(handle: QPixmapH; fileName: PWideString; format: PAnsiChar = 0; flags: QtImageConversionFlags = QtAutoColor): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_load';
function QPixmap_loadFromData(handle: QPixmapH; buf: PByte; len: Cardinal; format: PAnsiChar = 0; flags: QtImageConversionFlags = QtAutoColor): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_loadFromData';
function QPixmap_loadFromData(handle: QPixmapH; data: QByteArrayH; format: PAnsiChar = 0; flags: QtImageConversionFlags = QtAutoColor): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_loadFromData2';
function QPixmap_save(handle: QPixmapH; fileName: PWideString; format: PAnsiChar; quality: Integer = -1): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_save';
function QPixmap_save(handle: QPixmapH; device: QIODeviceH; format: PAnsiChar; quality: Integer = -1): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_save2';
function QPixmap_toWinHBITMAP(handle: QPixmapH; format: QPixmapHBitmapFormat = QPixmapNoAlpha): HBITMAP; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_toWinHBITMAP';
procedure QPixmap_fromWinHBITMAP(retval: QPixmapH; hbitmap: HBITMAP; format: QPixmapHBitmapFormat = QPixmapNoAlpha); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_fromWinHBITMAP';
procedure QPixmap_copy(handle: QPixmapH; retval: QPixmapH; x: Integer; y: Integer; width: Integer; height: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_copy';
procedure QPixmap_copy(handle: QPixmapH; retval: QPixmapH; rect: PRect = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_copy2';
function QPixmap_serialNumber(handle: QPixmapH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_serialNumber';
function QPixmap_isDetached(handle: QPixmapH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_isDetached';
procedure QPixmap_detach(handle: QPixmapH); cdecl; external QtShareName name QtNamePrefix + 'QPixmap_detach';
function QPixmap_isQBitmap(handle: QPixmapH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_isQBitmap';
function QPixmap_paintEngine(handle: QPixmapH): QPaintEngineH; cdecl; external QtShareName name QtNamePrefix + 'QPixmap_paintEngine';


type
  QImageInvertMode = ( // QImage::InvertMode (1)
    QImageInvertRgb, QImageInvertRgba );

  QImageFormat = ( // QImage::Format (1)
    QImageFormat_Invalid, QImageFormat_Mono, QImageFormat_MonoLSB, QImageFormat_Indexed8, QImageFormat_RGB32, QImageFormat_ARGB32, QImageFormat_ARGB32_Premultiplied, QImageNImageFormats );

function QImage_create(): QImageH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_create';
procedure QImage_destroy(handle: QImageH); cdecl; external QtShareName name QtNamePrefix + 'QImage_destroy'; 
function QImage_create(size: PSize; format: QImageFormat): QImageH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_create2';
function QImage_create(width: Integer; height: Integer; format: QImageFormat): QImageH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_create3';
function QImage_create(data: PByte; width: Integer; height: Integer; format: QImageFormat): QImageH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_create4';
function QImage_create(xpm: PAnsiChar): QImageH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_create5';
function QImage_create(fileName: PWideString; format: PAnsiChar = 0): QImageH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_create6';
function QImage_create(fileName: PAnsiChar; format: PAnsiChar = 0): QImageH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_create7';
function QImage_create(p1: QImageH): QImageH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_create8';
function QImage_isNull(handle: QImageH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImage_isNull';
function QImage_devType(handle: QImageH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImage_devType';
procedure QImage_detach(handle: QImageH); cdecl; external QtShareName name QtNamePrefix + 'QImage_detach';
function QImage_isDetached(handle: QImageH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImage_isDetached';
procedure QImage_copy(handle: QImageH; retval: QImageH; rect: PRect = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_copy';
procedure QImage_copy(handle: QImageH; retval: QImageH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_copy2';
function QImage_format(handle: QImageH): QImageFormat; cdecl; external QtShareName name QtNamePrefix + 'QImage_format';
procedure QImage_convertToFormat(handle: QImageH; retval: QImageH; f: QImageFormat; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_convertToFormat';
function QImage_width(handle: QImageH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImage_width';
function QImage_height(handle: QImageH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImage_height';
procedure QImage_size(handle: QImageH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QImage_size';
procedure QImage_rect(handle: QImageH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QImage_rect';
function QImage_depth(handle: QImageH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImage_depth';
function QImage_numColors(handle: QImageH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImage_numColors';
function QImage_color(handle: QImageH; i: Integer): QRgb; cdecl; external QtShareName name QtNamePrefix + 'QImage_color';
procedure QImage_setColor(handle: QImageH; i: Integer; c: QRgb); cdecl; external QtShareName name QtNamePrefix + 'QImage_setColor';
procedure QImage_setNumColors(handle: QImageH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QImage_setNumColors';
function QImage_allGray(handle: QImageH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImage_allGray';
function QImage_isGrayscale(handle: QImageH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImage_isGrayscale';
function QImage_bits(handle: QImageH): PByte; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_bits';
function QImage_numBytes(handle: QImageH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImage_numBytes';
function QImage_scanLine(handle: QImageH; p1: Integer): PByte; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_scanLine';
function QImage_bytesPerLine(handle: QImageH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImage_bytesPerLine';
function QImage_valid(handle: QImageH; x: Integer; y: Integer): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImage_valid';
function QImage_pixelIndex(handle: QImageH; x: Integer; y: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImage_pixelIndex';
function QImage_pixel(handle: QImageH; x: Integer; y: Integer): QRgb; cdecl; external QtShareName name QtNamePrefix + 'QImage_pixel';
procedure QImage_setPixel(handle: QImageH; x: Integer; y: Integer; index_or_rgb: Cardinal); cdecl; external QtShareName name QtNamePrefix + 'QImage_setPixel';
procedure QImage_fill(handle: QImageH; pixel: Cardinal); cdecl; external QtShareName name QtNamePrefix + 'QImage_fill';
function QImage_hasAlphaChannel(handle: QImageH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImage_hasAlphaChannel';
procedure QImage_setAlphaChannel(handle: QImageH; alphaChannel: QImageH); cdecl; external QtShareName name QtNamePrefix + 'QImage_setAlphaChannel';
procedure QImage_alphaChannel(handle: QImageH; retval: QImageH); cdecl; external QtShareName name QtNamePrefix + 'QImage_alphaChannel';
procedure QImage_createAlphaMask(handle: QImageH; retval: QImageH; flags: QtImageConversionFlags = QtAutoColor); cdecl; external QtShareName name QtNamePrefix + 'QImage_createAlphaMask';
procedure QImage_createHeuristicMask(handle: QImageH; retval: QImageH; clipTight: Boolean = True); cdecl; external QtShareName name QtNamePrefix + 'QImage_createHeuristicMask';
procedure QImage_scaled(handle: QImageH; retval: QImageH; w: Integer; h: Integer; aspectMode: QtAspectRatioMode = QtIgnoreAspectRatio; mode: QtTransformationMode = QtFastTransformation); overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_scaled';
procedure QImage_scaled(handle: QImageH; retval: QImageH; s: PSize; aspectMode: QtAspectRatioMode = QtIgnoreAspectRatio; mode: QtTransformationMode = QtFastTransformation); overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_scaled2';
procedure QImage_scaledToWidth(handle: QImageH; retval: QImageH; w: Integer; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtShareName name QtNamePrefix + 'QImage_scaledToWidth';
procedure QImage_scaledToHeight(handle: QImageH; retval: QImageH; h: Integer; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtShareName name QtNamePrefix + 'QImage_scaledToHeight';
procedure QImage_transformed(handle: QImageH; retval: QImageH; matrix: QMatrixH; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtShareName name QtNamePrefix + 'QImage_transformed';
procedure QImage_trueMatrix(retval: QMatrixH; p1: QMatrixH; w: Integer; h: Integer); cdecl; external QtShareName name QtNamePrefix + 'QImage_trueMatrix';
procedure QImage_mirrored(handle: QImageH; retval: QImageH; horizontally: Boolean = False; vertically: Boolean = True); cdecl; external QtShareName name QtNamePrefix + 'QImage_mirrored';
procedure QImage_rgbSwapped(handle: QImageH; retval: QImageH); cdecl; external QtShareName name QtNamePrefix + 'QImage_rgbSwapped';
procedure QImage_invertPixels(handle: QImageH; p1: QImageInvertMode = QImageInvertRgb); cdecl; external QtShareName name QtNamePrefix + 'QImage_invertPixels';
function QImage_load(handle: QImageH; device: QIODeviceH; format: PAnsiChar): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_load';
function QImage_load(handle: QImageH; fileName: PWideString; format: PAnsiChar = 0): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_load2';
function QImage_loadFromData(handle: QImageH; buf: PByte; len: Integer; format: PAnsiChar = 0): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_loadFromData';
function QImage_loadFromData(handle: QImageH; data: QByteArrayH; aformat: PAnsiChar = 0): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_loadFromData2';
function QImage_save(handle: QImageH; fileName: PWideString; format: PAnsiChar; quality: Integer = -1): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_save';
function QImage_save(handle: QImageH; device: QIODeviceH; format: PAnsiChar; quality: Integer = -1): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_save2';
procedure QImage_fromData(retval: QImageH; data: PByte; size: Integer; format: PAnsiChar = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_fromData';
procedure QImage_fromData(retval: QImageH; data: QByteArrayH; format: PAnsiChar = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_fromData2';
function QImage_serialNumber(handle: QImageH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImage_serialNumber';
function QImage_paintEngine(handle: QImageH): QPaintEngineH; cdecl; external QtShareName name QtNamePrefix + 'QImage_paintEngine';
function QImage_dotsPerMeterX(handle: QImageH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImage_dotsPerMeterX';
function QImage_dotsPerMeterY(handle: QImageH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImage_dotsPerMeterY';
procedure QImage_setDotsPerMeterX(handle: QImageH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QImage_setDotsPerMeterX';
procedure QImage_setDotsPerMeterY(handle: QImageH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QImage_setDotsPerMeterY';
procedure QImage_offset(handle: QImageH; retval: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QImage_offset';
procedure QImage_setOffset(handle: QImageH; p1: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QImage_setOffset';
procedure QImage_textKeys(handle: QImageH; retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QImage_textKeys';
procedure QImage_text(handle: QImageH; retval: PWideString; key: PWideString = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_text';
procedure QImage_setText(handle: QImageH; key: PWideString; value: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_setText';
procedure QImage_text(handle: QImageH; retval: PWideString; key: PAnsiChar; lang: PAnsiChar = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_text2';
procedure QImage_textLanguages(handle: QImageH; retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QImage_textLanguages';
procedure QImage_setText(handle: QImageH; key: PAnsiChar; lang: PAnsiChar; p3: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QImage_setText2';

function QBitmap_create(): QBitmapH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBitmap_create';
procedure QBitmap_destroy(handle: QBitmapH); cdecl; external QtShareName name QtNamePrefix + 'QBitmap_destroy'; 
function QBitmap_create(p1: QPixmapH): QBitmapH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBitmap_create2';
function QBitmap_create(w: Integer; h: Integer): QBitmapH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBitmap_create3';
function QBitmap_create(p1: PSize): QBitmapH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBitmap_create4';
function QBitmap_create(fileName: PWideString; format: PAnsiChar = 0): QBitmapH; overload; cdecl; external QtShareName name QtNamePrefix + 'QBitmap_create5';
procedure QBitmap_clear(handle: QBitmapH); cdecl; external QtShareName name QtNamePrefix + 'QBitmap_clear';
procedure QBitmap_fromImage(retval: QBitmapH; image: QImageH; flags: QtImageConversionFlags = QtAutoColor); cdecl; external QtShareName name QtNamePrefix + 'QBitmap_fromImage';
procedure QBitmap_fromData(retval: QBitmapH; size: PSize; bits: PByte; monoFormat: QImageFormat = QImageFormat_MonoLSB); cdecl; external QtShareName name QtNamePrefix + 'QBitmap_fromData';
procedure QBitmap_transformed(handle: QBitmapH; retval: QBitmapH; p1: QMatrixH); cdecl; external QtShareName name QtNamePrefix + 'QBitmap_transformed';

function QPicture_create(formatVersion: Integer = -1): QPictureH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPicture_create';
procedure QPicture_destroy(handle: QPictureH); cdecl; external QtShareName name QtNamePrefix + 'QPicture_destroy'; 
function QPicture_create(p1: QPictureH): QPictureH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPicture_create2';
function QPicture_isNull(handle: QPictureH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPicture_isNull';
function QPicture_devType(handle: QPictureH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPicture_devType';
function QPicture_size(handle: QPictureH): Cardinal; cdecl; external QtShareName name QtNamePrefix + 'QPicture_size';
function QPicture_data(handle: QPictureH): PAnsiChar; cdecl; external QtShareName name QtNamePrefix + 'QPicture_data';
procedure QPicture_setData(handle: QPictureH; data: PAnsiChar; size: Cardinal); cdecl; external QtShareName name QtNamePrefix + 'QPicture_setData';
function QPicture_play(handle: QPictureH; p: QPainterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPicture_play';
function QPicture_load(handle: QPictureH; dev: QIODeviceH; format: PAnsiChar = 0): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QPicture_load';
function QPicture_load(handle: QPictureH; fileName: PWideString; format: PAnsiChar = 0): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QPicture_load2';
function QPicture_save(handle: QPictureH; dev: QIODeviceH; format: PAnsiChar = 0): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QPicture_save';
function QPicture_save(handle: QPictureH; fileName: PWideString; format: PAnsiChar = 0): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QPicture_save2';
procedure QPicture_boundingRect(handle: QPictureH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QPicture_boundingRect';
procedure QPicture_setBoundingRect(handle: QPictureH; r: PRect); cdecl; external QtShareName name QtNamePrefix + 'QPicture_setBoundingRect';
procedure QPicture_detach(handle: QPictureH); cdecl; external QtShareName name QtNamePrefix + 'QPicture_detach';
function QPicture_isDetached(handle: QPictureH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPicture_isDetached';
function QPicture_pictureFormat(fileName: PWideString): PAnsiChar; cdecl; external QtShareName name QtNamePrefix + 'QPicture_pictureFormat';
procedure QPicture_inputFormatList(retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QPicture_inputFormatList';
procedure QPicture_outputFormatList(retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QPicture_outputFormatList';
function QPicture_paintEngine(handle: QPictureH): QPaintEngineH; cdecl; external QtShareName name QtNamePrefix + 'QPicture_paintEngine';

function QPictureIO_create(): QPictureIOH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_create';
procedure QPictureIO_destroy(handle: QPictureIOH); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_destroy'; 
function QPictureIO_create(ioDevice: QIODeviceH; format: PAnsiChar): QPictureIOH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_create2';
function QPictureIO_create(fileName: PWideString; format: PAnsiChar): QPictureIOH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_create3';
function QPictureIO_picture(handle: QPictureIOH): QPictureH; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_picture';
function QPictureIO_status(handle: QPictureIOH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_status';
function QPictureIO_format(handle: QPictureIOH): PAnsiChar; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_format';
function QPictureIO_ioDevice(handle: QPictureIOH): QIODeviceH; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_ioDevice';
procedure QPictureIO_fileName(handle: QPictureIOH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_fileName';
function QPictureIO_quality(handle: QPictureIOH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_quality';
procedure QPictureIO_description(handle: QPictureIOH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_description';
function QPictureIO_parameters(handle: QPictureIOH): PAnsiChar; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_parameters';
function QPictureIO_gamma(handle: QPictureIOH): Single; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_gamma';
procedure QPictureIO_setPicture(handle: QPictureIOH; p1: QPictureH); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_setPicture';
procedure QPictureIO_setStatus(handle: QPictureIOH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_setStatus';
procedure QPictureIO_setFormat(handle: QPictureIOH; p1: PAnsiChar); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_setFormat';
procedure QPictureIO_setIODevice(handle: QPictureIOH; p1: QIODeviceH); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_setIODevice';
procedure QPictureIO_setFileName(handle: QPictureIOH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_setFileName';
procedure QPictureIO_setQuality(handle: QPictureIOH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_setQuality';
procedure QPictureIO_setDescription(handle: QPictureIOH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_setDescription';
procedure QPictureIO_setParameters(handle: QPictureIOH; p1: PAnsiChar); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_setParameters';
procedure QPictureIO_setGamma(handle: QPictureIOH; p1: Single); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_setGamma';
function QPictureIO_read(handle: QPictureIOH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_read';
function QPictureIO_write(handle: QPictureIOH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_write';
procedure QPictureIO_pictureFormat(retval: QByteArrayH; fileName: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_pictureFormat';
procedure QPictureIO_pictureFormat(retval: QByteArrayH; p1: QIODeviceH); overload; cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_pictureFormat2';
procedure QPictureIO_defineIOHandler(format: PAnsiChar; header: PAnsiChar; flags: PAnsiChar; read_picture: TPictureIOHandler; write_picture: TPictureIOHandler); cdecl; external QtShareName name QtNamePrefix + 'QPictureIO_defineIOHandler';


type
  QImageReaderImageReaderError = ( // QImageReader::ImageReaderError (1)
    QImageReaderUnknownError, QImageReaderFileNotFoundError, QImageReaderDeviceError, QImageReaderUnsupportedFormatError, QImageReaderInvalidDataError );

function QImageReader_create(): QImageReaderH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_create';
procedure QImageReader_destroy(handle: QImageReaderH); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_destroy'; 
function QImageReader_create(device: QIODeviceH; format: QByteArrayH = nil): QImageReaderH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_create2';
function QImageReader_create(fileName: PWideString; format: QByteArrayH = nil): QImageReaderH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_create3';
procedure QImageReader_setFormat(handle: QImageReaderH; format: QByteArrayH); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_setFormat';
procedure QImageReader_format(handle: QImageReaderH; retval: QByteArrayH); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_format';
procedure QImageReader_setDevice(handle: QImageReaderH; device: QIODeviceH); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_setDevice';
function QImageReader_device(handle: QImageReaderH): QIODeviceH; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_device';
procedure QImageReader_setFileName(handle: QImageReaderH; fileName: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_setFileName';
procedure QImageReader_fileName(handle: QImageReaderH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_fileName';
procedure QImageReader_size(handle: QImageReaderH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_size';
procedure QImageReader_textKeys(handle: QImageReaderH; retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_textKeys';
procedure QImageReader_text(handle: QImageReaderH; retval: PWideString; key: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_text';
procedure QImageReader_setClipRect(handle: QImageReaderH; rect: PRect); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_setClipRect';
procedure QImageReader_clipRect(handle: QImageReaderH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_clipRect';
procedure QImageReader_setScaledSize(handle: QImageReaderH; size: PSize); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_setScaledSize';
procedure QImageReader_scaledSize(handle: QImageReaderH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_scaledSize';
procedure QImageReader_setScaledClipRect(handle: QImageReaderH; rect: PRect); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_setScaledClipRect';
procedure QImageReader_scaledClipRect(handle: QImageReaderH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_scaledClipRect';
procedure QImageReader_setBackgroundColor(handle: QImageReaderH; color: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_setBackgroundColor';
procedure QImageReader_backgroundColor(handle: QImageReaderH; retval: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_backgroundColor';
function QImageReader_supportsAnimation(handle: QImageReaderH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_supportsAnimation';
function QImageReader_canRead(handle: QImageReaderH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_canRead';
procedure QImageReader_read(handle: QImageReaderH; retval: QImageH); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_read';
function QImageReader_jumpToNextImage(handle: QImageReaderH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_jumpToNextImage';
function QImageReader_jumpToImage(handle: QImageReaderH; imageNumber: Integer): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_jumpToImage';
function QImageReader_loopCount(handle: QImageReaderH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_loopCount';
function QImageReader_imageCount(handle: QImageReaderH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_imageCount';
function QImageReader_nextImageDelay(handle: QImageReaderH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_nextImageDelay';
function QImageReader_currentImageNumber(handle: QImageReaderH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_currentImageNumber';
procedure QImageReader_currentImageRect(handle: QImageReaderH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_currentImageRect';
function QImageReader_error(handle: QImageReaderH): QImageReaderImageReaderError; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_error';
procedure QImageReader_errorString(handle: QImageReaderH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QImageReader_errorString';
procedure QImageReader_imageFormat(retval: QByteArrayH; fileName: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_imageFormat';
procedure QImageReader_imageFormat(retval: QByteArrayH; device: QIODeviceH); overload; cdecl; external QtShareName name QtNamePrefix + 'QImageReader_imageFormat2';


type
  QImageWriterImageWriterError = ( // QImageWriter::ImageWriterError (1)
    QImageWriterUnknownError, QImageWriterDeviceError, QImageWriterUnsupportedFormatError );

function QImageWriter_create(): QImageWriterH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_create';
procedure QImageWriter_destroy(handle: QImageWriterH); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_destroy'; 
function QImageWriter_create(device: QIODeviceH; format: QByteArrayH): QImageWriterH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_create2';
function QImageWriter_create(fileName: PWideString; format: QByteArrayH = nil): QImageWriterH; overload; cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_create3';
procedure QImageWriter_setFormat(handle: QImageWriterH; format: QByteArrayH); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_setFormat';
procedure QImageWriter_format(handle: QImageWriterH; retval: QByteArrayH); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_format';
procedure QImageWriter_setDevice(handle: QImageWriterH; device: QIODeviceH); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_setDevice';
function QImageWriter_device(handle: QImageWriterH): QIODeviceH; cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_device';
procedure QImageWriter_setFileName(handle: QImageWriterH; fileName: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_setFileName';
procedure QImageWriter_fileName(handle: QImageWriterH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_fileName';
procedure QImageWriter_setQuality(handle: QImageWriterH; quality: Integer); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_setQuality';
function QImageWriter_quality(handle: QImageWriterH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_quality';
procedure QImageWriter_setGamma(handle: QImageWriterH; gamma: Single); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_setGamma';
function QImageWriter_gamma(handle: QImageWriterH): Single; cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_gamma';
procedure QImageWriter_setDescription(handle: QImageWriterH; description: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_setDescription';
procedure QImageWriter_description(handle: QImageWriterH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_description';
procedure QImageWriter_setText(handle: QImageWriterH; key: PWideString; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_setText';
function QImageWriter_canWrite(handle: QImageWriterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_canWrite';
function QImageWriter_write(handle: QImageWriterH; image: QImageH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_write';
function QImageWriter_error(handle: QImageWriterH): QImageWriterImageWriterError; cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_error';
procedure QImageWriter_errorString(handle: QImageWriterH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QImageWriter_errorString';


type
  QValidatorState = ( // QValidator::State (1)
    QValidatorInvalid, QValidatorIntermediate, QValidatorAcceptable );

function QValidator_validate(handle: QValidatorH; p1: PWideString; p2: PInteger): QValidatorState; cdecl; external QtShareName name QtNamePrefix + 'QValidator_validate';
procedure QValidator_fixup(handle: QValidatorH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QValidator_fixup';

function QIntValidator_create(parent: QObjectH): QIntValidatorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QIntValidator_create';
procedure QIntValidator_destroy(handle: QIntValidatorH); cdecl; external QtShareName name QtNamePrefix + 'QIntValidator_destroy'; 
function QIntValidator_create(bottom: Integer; top: Integer; parent: QObjectH): QIntValidatorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QIntValidator_create2';
function QIntValidator_validate(handle: QIntValidatorH; p1: PWideString; p2: PInteger): QValidatorState; cdecl; external QtShareName name QtNamePrefix + 'QIntValidator_validate';
procedure QIntValidator_setBottom(handle: QIntValidatorH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QIntValidator_setBottom';
procedure QIntValidator_setTop(handle: QIntValidatorH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QIntValidator_setTop';
procedure QIntValidator_setRange(handle: QIntValidatorH; bottom: Integer; top: Integer); cdecl; external QtShareName name QtNamePrefix + 'QIntValidator_setRange';
function QIntValidator_bottom(handle: QIntValidatorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QIntValidator_bottom';
function QIntValidator_top(handle: QIntValidatorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QIntValidator_top';

function QDoubleValidator_create(parent: QObjectH): QDoubleValidatorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QDoubleValidator_create';
procedure QDoubleValidator_destroy(handle: QDoubleValidatorH); cdecl; external QtShareName name QtNamePrefix + 'QDoubleValidator_destroy'; 
function QDoubleValidator_create(bottom: Double; top: Double; decimals: Integer; parent: QObjectH): QDoubleValidatorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QDoubleValidator_create2';
function QDoubleValidator_validate(handle: QDoubleValidatorH; p1: PWideString; p2: PInteger): QValidatorState; cdecl; external QtShareName name QtNamePrefix + 'QDoubleValidator_validate';
procedure QDoubleValidator_setRange(handle: QDoubleValidatorH; bottom: Double; top: Double; decimals: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QDoubleValidator_setRange';
procedure QDoubleValidator_setBottom(handle: QDoubleValidatorH; p1: Double); cdecl; external QtShareName name QtNamePrefix + 'QDoubleValidator_setBottom';
procedure QDoubleValidator_setTop(handle: QDoubleValidatorH; p1: Double); cdecl; external QtShareName name QtNamePrefix + 'QDoubleValidator_setTop';
procedure QDoubleValidator_setDecimals(handle: QDoubleValidatorH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QDoubleValidator_setDecimals';
function QDoubleValidator_bottom(handle: QDoubleValidatorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QDoubleValidator_bottom';
function QDoubleValidator_top(handle: QDoubleValidatorH): Double; cdecl; external QtShareName name QtNamePrefix + 'QDoubleValidator_top';
function QDoubleValidator_decimals(handle: QDoubleValidatorH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QDoubleValidator_decimals';

function QRegExpValidator_create(parent: QObjectH): QRegExpValidatorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRegExpValidator_create';
procedure QRegExpValidator_destroy(handle: QRegExpValidatorH); cdecl; external QtShareName name QtNamePrefix + 'QRegExpValidator_destroy'; 
function QRegExpValidator_create(rx: QRegExpH; parent: QObjectH): QRegExpValidatorH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRegExpValidator_create2';
function QRegExpValidator_validate(handle: QRegExpValidatorH; input: PWideString; pos: PInteger): QValidatorState; cdecl; external QtShareName name QtNamePrefix + 'QRegExpValidator_validate';
procedure QRegExpValidator_setRegExp(handle: QRegExpValidatorH; rx: QRegExpH); cdecl; external QtShareName name QtNamePrefix + 'QRegExpValidator_setRegExp';
function QRegExpValidator_regExp(handle: QRegExpValidatorH): QRegExpH; cdecl; external QtShareName name QtNamePrefix + 'QRegExpValidator_regExp';


type
  QFrameShape = (  //QFrame::Shape (2s)
    QFrameNoFrame = 0,
    QFrameBox = $0001,
    QFramePanel = $0002,
    QFrameWinPanel = $0003,
    QFrameHLine = $0004,
    QFrameVLine = $0005,
    QFrameStyledPanel = $0006 );

  QFrameShadow = (  //QFrame::Shadow (2s)
    QFramePlain = $0010,
    QFrameRaised = $0020,
    QFrameSunken = $0030 );

function QFrame_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QFrameH; cdecl; external QtShareName name QtNamePrefix + 'QFrame_create';
procedure QFrame_destroy(handle: QFrameH); cdecl; external QtShareName name QtNamePrefix + 'QFrame_destroy'; 
function QFrame_frameStyle(handle: QFrameH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFrame_frameStyle';
procedure QFrame_setFrameStyle(handle: QFrameH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QFrame_setFrameStyle';
function QFrame_frameWidth(handle: QFrameH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFrame_frameWidth';
procedure QFrame_sizeHint(handle: QFrameH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QFrame_sizeHint';
function QFrame_frameShape(handle: QFrameH): QFrameShape; cdecl; external QtShareName name QtNamePrefix + 'QFrame_frameShape';
procedure QFrame_setFrameShape(handle: QFrameH; p1: QFrameShape); cdecl; external QtShareName name QtNamePrefix + 'QFrame_setFrameShape';
function QFrame_frameShadow(handle: QFrameH): QFrameShadow; cdecl; external QtShareName name QtNamePrefix + 'QFrame_frameShadow';
procedure QFrame_setFrameShadow(handle: QFrameH; p1: QFrameShadow); cdecl; external QtShareName name QtNamePrefix + 'QFrame_setFrameShadow';
function QFrame_lineWidth(handle: QFrameH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFrame_lineWidth';
procedure QFrame_setLineWidth(handle: QFrameH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QFrame_setLineWidth';
function QFrame_midLineWidth(handle: QFrameH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QFrame_midLineWidth';
procedure QFrame_setMidLineWidth(handle: QFrameH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QFrame_setMidLineWidth';
procedure QFrame_frameRect(handle: QFrameH; retval: PRect); cdecl; external QtShareName name QtNamePrefix + 'QFrame_frameRect';
procedure QFrame_setFrameRect(handle: QFrameH; p1: PRect); cdecl; external QtShareName name QtNamePrefix + 'QFrame_setFrameRect';

function QAbstractScrollArea_create(parent: QWidgetH = nil): QAbstractScrollAreaH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_create';
procedure QAbstractScrollArea_destroy(handle: QAbstractScrollAreaH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_destroy'; 
function QAbstractScrollArea_verticalScrollBarPolicy(handle: QAbstractScrollAreaH): QtScrollBarPolicy; cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_verticalScrollBarPolicy';
procedure QAbstractScrollArea_setVerticalScrollBarPolicy(handle: QAbstractScrollAreaH; p1: QtScrollBarPolicy); cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_setVerticalScrollBarPolicy';
function QAbstractScrollArea_verticalScrollBar(handle: QAbstractScrollAreaH): QScrollBarH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_verticalScrollBar';
function QAbstractScrollArea_horizontalScrollBarPolicy(handle: QAbstractScrollAreaH): QtScrollBarPolicy; cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_horizontalScrollBarPolicy';
procedure QAbstractScrollArea_setHorizontalScrollBarPolicy(handle: QAbstractScrollAreaH; p1: QtScrollBarPolicy); cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_setHorizontalScrollBarPolicy';
function QAbstractScrollArea_horizontalScrollBar(handle: QAbstractScrollAreaH): QScrollBarH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_horizontalScrollBar';
function QAbstractScrollArea_viewport(handle: QAbstractScrollAreaH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_viewport';
procedure QAbstractScrollArea_maximumViewportSize(handle: QAbstractScrollAreaH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_maximumViewportSize';
procedure QAbstractScrollArea_minimumSizeHint(handle: QAbstractScrollAreaH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_minimumSizeHint';
procedure QAbstractScrollArea_sizeHint(handle: QAbstractScrollAreaH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_sizeHint';


type
  QAbstractSliderSliderAction = ( // QAbstractSlider::SliderAction (1)
    QAbstractSliderSliderNoAction, QAbstractSliderSliderSingleStepAdd, QAbstractSliderSliderSingleStepSub, QAbstractSliderSliderPageStepAdd, QAbstractSliderSliderPageStepSub, QAbstractSliderSliderToMinimum, 
    QAbstractSliderSliderToMaximum, QAbstractSliderSliderMove );

function QAbstractSlider_create(parent: QWidgetH = nil): QAbstractSliderH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_create';
procedure QAbstractSlider_destroy(handle: QAbstractSliderH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_destroy'; 
function QAbstractSlider_orientation(handle: QAbstractSliderH): QtOrientation; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_orientation';
procedure QAbstractSlider_setMinimum(handle: QAbstractSliderH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setMinimum';
function QAbstractSlider_minimum(handle: QAbstractSliderH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_minimum';
procedure QAbstractSlider_setMaximum(handle: QAbstractSliderH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setMaximum';
function QAbstractSlider_maximum(handle: QAbstractSliderH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_maximum';
procedure QAbstractSlider_setRange(handle: QAbstractSliderH; min: Integer; max: Integer); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setRange';
procedure QAbstractSlider_setSingleStep(handle: QAbstractSliderH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setSingleStep';
function QAbstractSlider_singleStep(handle: QAbstractSliderH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_singleStep';
procedure QAbstractSlider_setPageStep(handle: QAbstractSliderH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setPageStep';
function QAbstractSlider_pageStep(handle: QAbstractSliderH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_pageStep';
procedure QAbstractSlider_setTracking(handle: QAbstractSliderH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setTracking';
function QAbstractSlider_hasTracking(handle: QAbstractSliderH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_hasTracking';
procedure QAbstractSlider_setSliderDown(handle: QAbstractSliderH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setSliderDown';
function QAbstractSlider_isSliderDown(handle: QAbstractSliderH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_isSliderDown';
procedure QAbstractSlider_setSliderPosition(handle: QAbstractSliderH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setSliderPosition';
function QAbstractSlider_sliderPosition(handle: QAbstractSliderH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_sliderPosition';
procedure QAbstractSlider_setInvertedAppearance(handle: QAbstractSliderH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setInvertedAppearance';
function QAbstractSlider_invertedAppearance(handle: QAbstractSliderH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_invertedAppearance';
procedure QAbstractSlider_setInvertedControls(handle: QAbstractSliderH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setInvertedControls';
function QAbstractSlider_invertedControls(handle: QAbstractSliderH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_invertedControls';
function QAbstractSlider_value(handle: QAbstractSliderH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_value';
procedure QAbstractSlider_triggerAction(handle: QAbstractSliderH; action: QAbstractSliderSliderAction); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_triggerAction';
procedure QAbstractSlider_setValue(handle: QAbstractSliderH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setValue';
procedure QAbstractSlider_setOrientation(handle: QAbstractSliderH; p1: QtOrientation); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_setOrientation';


type
  QAbstractSlider_valueChanged_Event = procedure (value: Integer) of object cdecl;
  QAbstractSlider_sliderPressed_Event = procedure () of object cdecl;
  QAbstractSlider_sliderMoved_Event = procedure (position: Integer) of object cdecl;
  QAbstractSlider_sliderReleased_Event = procedure () of object cdecl;
  QAbstractSlider_rangeChanged_Event = procedure (min: Integer; max: Integer) of object cdecl;
  QAbstractSlider_actionTriggered_Event = procedure (action: Integer) of object cdecl;


function QScrollBar_create(parent: QWidgetH = nil): QScrollBarH; overload; cdecl; external QtShareName name QtNamePrefix + 'QScrollBar_create';
procedure QScrollBar_destroy(handle: QScrollBarH); cdecl; external QtShareName name QtNamePrefix + 'QScrollBar_destroy'; 
function QScrollBar_create(p1: QtOrientation; parent: QWidgetH = nil): QScrollBarH; overload; cdecl; external QtShareName name QtNamePrefix + 'QScrollBar_create2';
procedure QScrollBar_sizeHint(handle: QScrollBarH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QScrollBar_sizeHint';
function QScrollBar_event(handle: QScrollBarH; event: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QScrollBar_event';

function QMenu_create(parent: QWidgetH = nil): QMenuH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_create';
procedure QMenu_destroy(handle: QMenuH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_destroy'; 
function QMenu_create(title: PWideString; parent: QWidgetH = nil): QMenuH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_create2';
function QMenu_addAction(handle: QMenuH; text: PWideString): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_addAction';
function QMenu_addAction(handle: QMenuH; icon: QIconH; text: PWideString): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_addAction2';
function QMenu_addAction(handle: QMenuH; text: PWideString; receiver: QObjectH; member: PAnsiChar; shortcut: QKeySequenceH = nil): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_addAction3';
function QMenu_addAction(handle: QMenuH; icon: QIconH; text: PWideString; receiver: QObjectH; member: PAnsiChar; shortcut: QKeySequenceH = nil): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_addAction4';
function QMenu_addMenu(handle: QMenuH; menu: QMenuH): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_addMenu';
function QMenu_addMenu(handle: QMenuH; title: PWideString): QMenuH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_addMenu2';
function QMenu_addMenu(handle: QMenuH; icon: QIconH; title: PWideString): QMenuH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_addMenu3';
function QMenu_addSeparator(handle: QMenuH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QMenu_addSeparator';
function QMenu_insertMenu(handle: QMenuH; before: QActionH; menu: QMenuH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QMenu_insertMenu';
function QMenu_insertSeparator(handle: QMenuH; before: QActionH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QMenu_insertSeparator';
procedure QMenu_clear(handle: QMenuH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_clear';
procedure QMenu_setTearOffEnabled(handle: QMenuH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QMenu_setTearOffEnabled';
function QMenu_isTearOffEnabled(handle: QMenuH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QMenu_isTearOffEnabled';
function QMenu_isTearOffMenuVisible(handle: QMenuH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QMenu_isTearOffMenuVisible';
procedure QMenu_hideTearOffMenu(handle: QMenuH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_hideTearOffMenu';
procedure QMenu_setDefaultAction(handle: QMenuH; p1: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_setDefaultAction';
function QMenu_defaultAction(handle: QMenuH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QMenu_defaultAction';
procedure QMenu_setActiveAction(handle: QMenuH; act: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_setActiveAction';
function QMenu_activeAction(handle: QMenuH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QMenu_activeAction';
procedure QMenu_popup(handle: QMenuH; pos: PPoint; at: QActionH = nil); cdecl; external QtShareName name QtNamePrefix + 'QMenu_popup';
function QMenu_exec(handle: QMenuH): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_exec';
function QMenu_exec(handle: QMenuH; pos: PPoint; at: QActionH = nil): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_exec2';
function QMenu_exec(actions: PIntArray; pos: PPoint; at: QActionH = nil): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenu_exec3';
procedure QMenu_sizeHint(handle: QMenuH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QMenu_sizeHint';
procedure QMenu_actionGeometry(handle: QMenuH; retval: PRect; p1: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_actionGeometry';
function QMenu_actionAt(handle: QMenuH; p1: PPoint): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QMenu_actionAt';
function QMenu_menuAction(handle: QMenuH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QMenu_menuAction';
procedure QMenu_title(handle: QMenuH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QMenu_title';
procedure QMenu_setTitle(handle: QMenuH; title: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QMenu_setTitle';
procedure QMenu_icon(handle: QMenuH; retval: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_icon';
procedure QMenu_setIcon(handle: QMenuH; icon: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_setIcon';
procedure QMenu_setNoReplayFor(handle: QMenuH; widget: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_setNoReplayFor';


type
  QMenu_aboutToShow_Event = procedure () of object cdecl;
  QMenu_triggered_Event = procedure (action: QActionH) of object cdecl;
  QMenu_hovered_Event = procedure (action: QActionH) of object cdecl;


function QMenuBar_create(parent: QWidgetH = nil): QMenuBarH; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_create';
procedure QMenuBar_destroy(handle: QMenuBarH); cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_destroy'; 
function QMenuBar_addAction(handle: QMenuBarH; text: PWideString): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_addAction';
function QMenuBar_addAction(handle: QMenuBarH; text: PWideString; receiver: QObjectH; member: PAnsiChar): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_addAction2';
function QMenuBar_addMenu(handle: QMenuBarH; menu: QMenuH): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_addMenu';
function QMenuBar_addMenu(handle: QMenuBarH; title: PWideString): QMenuH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_addMenu2';
function QMenuBar_addMenu(handle: QMenuBarH; icon: QIconH; title: PWideString): QMenuH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_addMenu3';
function QMenuBar_addSeparator(handle: QMenuBarH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_addSeparator';
function QMenuBar_insertMenu(handle: QMenuBarH; before: QActionH; menu: QMenuH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_insertMenu';
procedure QMenuBar_clear(handle: QMenuBarH); cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_clear';
function QMenuBar_activeAction(handle: QMenuBarH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_activeAction';
procedure QMenuBar_setActiveAction(handle: QMenuBarH; action: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_setActiveAction';
procedure QMenuBar_setDefaultUp(handle: QMenuBarH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_setDefaultUp';
function QMenuBar_isDefaultUp(handle: QMenuBarH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_isDefaultUp';
procedure QMenuBar_sizeHint(handle: QMenuBarH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_sizeHint';
procedure QMenuBar_minimumSizeHint(handle: QMenuBarH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_minimumSizeHint';
function QMenuBar_heightForWidth(handle: QMenuBarH; p1: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_heightForWidth';
procedure QMenuBar_actionGeometry(handle: QMenuBarH; retval: PRect; p1: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_actionGeometry';
function QMenuBar_actionAt(handle: QMenuBarH; p1: PPoint): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_actionAt';
procedure QMenuBar_setCornerWidget(handle: QMenuBarH; w: QWidgetH; corner: QtCorner = QtTopRightCorner); cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_setCornerWidget';
function QMenuBar_cornerWidget(handle: QMenuBarH; corner: QtCorner = QtTopRightCorner): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_cornerWidget';


type
  QMenuBar_triggered_Event = procedure (action: QActionH) of object cdecl;
  QMenuBar_hovered_Event = procedure (action: QActionH) of object cdecl;


function QButtonGroup_create(parent: QObjectH = nil): QButtonGroupH; cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_create';
procedure QButtonGroup_destroy(handle: QButtonGroupH); cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_destroy'; 
procedure QButtonGroup_setExclusive(handle: QButtonGroupH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_setExclusive';
function QButtonGroup_exclusive(handle: QButtonGroupH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_exclusive';
procedure QButtonGroup_addButton(handle: QButtonGroupH; p1: QAbstractButtonH); overload; cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_addButton';
procedure QButtonGroup_addButton(handle: QButtonGroupH; p1: QAbstractButtonH; id: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_addButton2';
procedure QButtonGroup_removeButton(handle: QButtonGroupH; p1: QAbstractButtonH); cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_removeButton';
procedure QButtonGroup_buttons(handle: QButtonGroupH; retval: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_buttons';
function QButtonGroup_checkedButton(handle: QButtonGroupH): QAbstractButtonH; cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_checkedButton';
function QButtonGroup_button(handle: QButtonGroupH; id: Integer): QAbstractButtonH; cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_button';
procedure QButtonGroup_setId(handle: QButtonGroupH; button: QAbstractButtonH; id: Integer); cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_setId';
function QButtonGroup_id(handle: QButtonGroupH; button: QAbstractButtonH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_id';
function QButtonGroup_checkedId(handle: QButtonGroupH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_checkedId';


type
  QButtonGroup_buttonClicked_Event = procedure (p1: QAbstractButtonH) of object cdecl;
  QButtonGroup_buttonClicked2_Event = procedure (p1: Integer) of object cdecl;


procedure QAbstractButton_setText(handle: QAbstractButtonH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_setText';
procedure QAbstractButton_text(handle: QAbstractButtonH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_text';
procedure QAbstractButton_setIcon(handle: QAbstractButtonH; icon: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_setIcon';
procedure QAbstractButton_icon(handle: QAbstractButtonH; retval: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_icon';
procedure QAbstractButton_iconSize(handle: QAbstractButtonH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_iconSize';
procedure QAbstractButton_setShortcut(handle: QAbstractButtonH; key: QKeySequenceH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_setShortcut';
procedure QAbstractButton_shortcut(handle: QAbstractButtonH; retval: QKeySequenceH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_shortcut';
procedure QAbstractButton_setCheckable(handle: QAbstractButtonH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_setCheckable';
function QAbstractButton_isCheckable(handle: QAbstractButtonH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_isCheckable';
function QAbstractButton_isChecked(handle: QAbstractButtonH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_isChecked';
procedure QAbstractButton_setDown(handle: QAbstractButtonH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_setDown';
function QAbstractButton_isDown(handle: QAbstractButtonH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_isDown';
procedure QAbstractButton_setAutoRepeat(handle: QAbstractButtonH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_setAutoRepeat';
function QAbstractButton_autoRepeat(handle: QAbstractButtonH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_autoRepeat';
procedure QAbstractButton_setAutoExclusive(handle: QAbstractButtonH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_setAutoExclusive';
function QAbstractButton_autoExclusive(handle: QAbstractButtonH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_autoExclusive';
function QAbstractButton_group(handle: QAbstractButtonH): QButtonGroupH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_group';
procedure QAbstractButton_setIconSize(handle: QAbstractButtonH; size: PSize); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_setIconSize';
procedure QAbstractButton_animateClick(handle: QAbstractButtonH; msec: Integer = 100); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_animateClick';
procedure QAbstractButton_click(handle: QAbstractButtonH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_click';
procedure QAbstractButton_toggle(handle: QAbstractButtonH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_toggle';
procedure QAbstractButton_setChecked(handle: QAbstractButtonH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_setChecked';


type
  QAbstractButton_pressed_Event = procedure () of object cdecl;
  QAbstractButton_released_Event = procedure () of object cdecl;
  QAbstractButton_clicked_Event = procedure (checked: Boolean = False) of object cdecl;
  QAbstractButton_clicked2_Event = procedure () of object cdecl;
  QAbstractButton_toggled_Event = procedure (checked: Boolean) of object cdecl;


function QPushButton_create(parent: QWidgetH = nil): QPushButtonH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPushButton_create';
procedure QPushButton_destroy(handle: QPushButtonH); cdecl; external QtShareName name QtNamePrefix + 'QPushButton_destroy'; 
function QPushButton_create(text: PWideString; parent: QWidgetH = nil): QPushButtonH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPushButton_create2';
function QPushButton_create(icon: QIconH; text: PWideString; parent: QWidgetH = nil): QPushButtonH; overload; cdecl; external QtShareName name QtNamePrefix + 'QPushButton_create3';
procedure QPushButton_sizeHint(handle: QPushButtonH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QPushButton_sizeHint';
function QPushButton_autoDefault(handle: QPushButtonH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPushButton_autoDefault';
procedure QPushButton_setAutoDefault(handle: QPushButtonH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QPushButton_setAutoDefault';
function QPushButton_isDefault(handle: QPushButtonH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPushButton_isDefault';
procedure QPushButton_setDefault(handle: QPushButtonH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QPushButton_setDefault';
procedure QPushButton_setMenu(handle: QPushButtonH; menu: QMenuH); cdecl; external QtShareName name QtNamePrefix + 'QPushButton_setMenu';
function QPushButton_menu(handle: QPushButtonH): QMenuH; cdecl; external QtShareName name QtNamePrefix + 'QPushButton_menu';
procedure QPushButton_setFlat(handle: QPushButtonH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QPushButton_setFlat';
function QPushButton_isFlat(handle: QPushButtonH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QPushButton_isFlat';
procedure QPushButton_showMenu(handle: QPushButtonH); cdecl; external QtShareName name QtNamePrefix + 'QPushButton_showMenu';

function QRadioButton_create(parent: QWidgetH = nil): QRadioButtonH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRadioButton_create';
procedure QRadioButton_destroy(handle: QRadioButtonH); cdecl; external QtShareName name QtNamePrefix + 'QRadioButton_destroy'; 
function QRadioButton_create(text: PWideString; parent: QWidgetH = nil): QRadioButtonH; overload; cdecl; external QtShareName name QtNamePrefix + 'QRadioButton_create2';
procedure QRadioButton_sizeHint(handle: QRadioButtonH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QRadioButton_sizeHint';


type
  QLineEditEchoMode = ( // QLineEdit::EchoMode (1)
    QLineEditNormal, QLineEditNoEcho, QLineEditPassword );

function QLineEdit_create(parent: QWidgetH = nil): QLineEditH; overload; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_create';
procedure QLineEdit_destroy(handle: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_destroy'; 
function QLineEdit_create(p1: PWideString; parent: QWidgetH = nil): QLineEditH; overload; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_create2';
procedure QLineEdit_text(handle: QLineEditH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_text';
procedure QLineEdit_displayText(handle: QLineEditH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_displayText';
function QLineEdit_maxLength(handle: QLineEditH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_maxLength';
procedure QLineEdit_setMaxLength(handle: QLineEditH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setMaxLength';
procedure QLineEdit_setFrame(handle: QLineEditH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setFrame';
function QLineEdit_hasFrame(handle: QLineEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_hasFrame';
function QLineEdit_echoMode(handle: QLineEditH): QLineEditEchoMode; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_echoMode';
procedure QLineEdit_setEchoMode(handle: QLineEditH; p1: QLineEditEchoMode); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setEchoMode';
function QLineEdit_isReadOnly(handle: QLineEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_isReadOnly';
procedure QLineEdit_setReadOnly(handle: QLineEditH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setReadOnly';
procedure QLineEdit_setValidator(handle: QLineEditH; p1: QValidatorH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setValidator';
function QLineEdit_validator(handle: QLineEditH): QValidatorH; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_validator';
procedure QLineEdit_sizeHint(handle: QLineEditH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_sizeHint';
procedure QLineEdit_minimumSizeHint(handle: QLineEditH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_minimumSizeHint';
function QLineEdit_cursorPosition(handle: QLineEditH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_cursorPosition';
procedure QLineEdit_setCursorPosition(handle: QLineEditH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setCursorPosition';
function QLineEdit_cursorPositionAt(handle: QLineEditH; pos: PPoint): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_cursorPositionAt';
procedure QLineEdit_setAlignment(handle: QLineEditH; flag: QtAlignment); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setAlignment';
function QLineEdit_alignment(handle: QLineEditH): QtAlignment; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_alignment';
procedure QLineEdit_cursorForward(handle: QLineEditH; mark: Boolean; steps: Integer = 1); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_cursorForward';
procedure QLineEdit_cursorBackward(handle: QLineEditH; mark: Boolean; steps: Integer = 1); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_cursorBackward';
procedure QLineEdit_cursorWordForward(handle: QLineEditH; mark: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_cursorWordForward';
procedure QLineEdit_cursorWordBackward(handle: QLineEditH; mark: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_cursorWordBackward';
procedure QLineEdit_backspace(handle: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_backspace';
procedure QLineEdit_del(handle: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_del';
procedure QLineEdit_home(handle: QLineEditH; mark: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_home';
procedure QLineEdit_end(handle: QLineEditH; mark: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_end';
function QLineEdit_isModified(handle: QLineEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_isModified';
procedure QLineEdit_setModified(handle: QLineEditH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setModified';
procedure QLineEdit_setSelection(handle: QLineEditH; p1: Integer; p2: Integer); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setSelection';
function QLineEdit_hasSelectedText(handle: QLineEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_hasSelectedText';
procedure QLineEdit_selectedText(handle: QLineEditH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_selectedText';
function QLineEdit_selectionStart(handle: QLineEditH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_selectionStart';
function QLineEdit_isUndoAvailable(handle: QLineEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_isUndoAvailable';
function QLineEdit_isRedoAvailable(handle: QLineEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_isRedoAvailable';
procedure QLineEdit_setDragEnabled(handle: QLineEditH; b: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setDragEnabled';
function QLineEdit_dragEnabled(handle: QLineEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_dragEnabled';
procedure QLineEdit_inputMask(handle: QLineEditH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_inputMask';
procedure QLineEdit_setInputMask(handle: QLineEditH; inputMask: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setInputMask';
function QLineEdit_hasAcceptableInput(handle: QLineEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_hasAcceptableInput';
procedure QLineEdit_setText(handle: QLineEditH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_setText';
procedure QLineEdit_clear(handle: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_clear';
procedure QLineEdit_selectAll(handle: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_selectAll';
procedure QLineEdit_undo(handle: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_undo';
procedure QLineEdit_redo(handle: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_redo';
procedure QLineEdit_cut(handle: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_cut';
procedure QLineEdit_copy(handle: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_copy';
procedure QLineEdit_paste(handle: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_paste';
procedure QLineEdit_deselect(handle: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_deselect';
procedure QLineEdit_insert(handle: QLineEditH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_insert';
function QLineEdit_createStandardContextMenu(handle: QLineEditH): QMenuH; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_createStandardContextMenu';
procedure QLineEdit_inputMethodQuery(handle: QLineEditH; retval: QVariantH; p1: QtInputMethodQuery); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_inputMethodQuery';
function QLineEdit_event(handle: QLineEditH; p1: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_event';


type
  QLineEdit_textChanged_Event = procedure (p1: PWideString) of object cdecl;
  QLineEdit_textEdited_Event = procedure (p1: PWideString) of object cdecl;
  QLineEdit_cursorPositionChanged_Event = procedure (p1: Integer; p2: Integer) of object cdecl;
  QLineEdit_returnPressed_Event = procedure () of object cdecl;
  QLineEdit_editingFinished_Event = procedure () of object cdecl;
  QLineEdit_selectionChanged_Event = procedure () of object cdecl;



type
  QTextEditLineWrapMode = ( // QTextEdit::LineWrapMode (1)
    QTextEditNoWrap, QTextEditWidgetWidth, QTextEditFixedPixelWidth, QTextEditFixedColumnWidth );

  QTextEditCursorAction = ( // QTextEdit::CursorAction (1)
    QTextEditMoveBackward, QTextEditMoveForward, QTextEditMoveWordBackward, QTextEditMoveWordForward, QTextEditMoveUp, QTextEditMoveDown, QTextEditMoveLineStart, QTextEditMoveLineEnd, QTextEditMoveHome, 
    QTextEditMoveEnd, QTextEditMovePageUp, QTextEditMovePageDown );

type
  QTextEditAutoFormattingFlag = cardinal; // QTextEdit::AutoFormattingFlag
  QTextEditAutoFormatting = QTextEditAutoFormattingFlag; //QFlags<> (3)
const
  QTextEditAutoNone =   0;
  QTextEditAutoBulletList =   $00000001;
  QTextEditAutoAll =   $ffffffff;

function QTextEdit_create(parent: QWidgetH = nil): QTextEditH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_create';
procedure QTextEdit_destroy(handle: QTextEditH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_destroy'; 
function QTextEdit_create(text: PWideString; parent: QWidgetH = nil): QTextEditH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_create2';
procedure QTextEdit_setDocument(handle: QTextEditH; document: QTextDocumentH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setDocument';
function QTextEdit_document(handle: QTextEditH): QTextDocumentH; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_document';
procedure QTextEdit_setTextCursor(handle: QTextEditH; cursor: QTextCursorH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setTextCursor';
procedure QTextEdit_textCursor(handle: QTextEditH; retval: QTextCursorH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_textCursor';
function QTextEdit_isReadOnly(handle: QTextEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_isReadOnly';
procedure QTextEdit_setReadOnly(handle: QTextEditH; ro: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setReadOnly';
function QTextEdit_fontPointSize(handle: QTextEditH): Double; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_fontPointSize';
procedure QTextEdit_fontFamily(handle: QTextEditH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_fontFamily';
function QTextEdit_fontWeight(handle: QTextEditH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_fontWeight';
function QTextEdit_fontUnderline(handle: QTextEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_fontUnderline';
function QTextEdit_fontItalic(handle: QTextEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_fontItalic';
procedure QTextEdit_textColor(handle: QTextEditH; retval: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_textColor';
procedure QTextEdit_currentFont(handle: QTextEditH; retval: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_currentFont';
function QTextEdit_alignment(handle: QTextEditH): QtAlignment; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_alignment';
procedure QTextEdit_mergeCurrentCharFormat(handle: QTextEditH; modifier: QTextCharFormatH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_mergeCurrentCharFormat';
procedure QTextEdit_setCurrentCharFormat(handle: QTextEditH; format: QTextCharFormatH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setCurrentCharFormat';
procedure QTextEdit_currentCharFormat(handle: QTextEditH; retval: QTextCharFormatH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_currentCharFormat';
function QTextEdit_autoFormatting(handle: QTextEditH): QTextEditAutoFormatting; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_autoFormatting';
procedure QTextEdit_setAutoFormatting(handle: QTextEditH; features: QTextEditAutoFormatting); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setAutoFormatting';
function QTextEdit_tabChangesFocus(handle: QTextEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_tabChangesFocus';
procedure QTextEdit_setTabChangesFocus(handle: QTextEditH; b: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setTabChangesFocus';
procedure QTextEdit_setDocumentTitle(handle: QTextEditH; title: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setDocumentTitle';
procedure QTextEdit_documentTitle(handle: QTextEditH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_documentTitle';
function QTextEdit_isUndoRedoEnabled(handle: QTextEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_isUndoRedoEnabled';
procedure QTextEdit_setUndoRedoEnabled(handle: QTextEditH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setUndoRedoEnabled';
function QTextEdit_lineWrapMode(handle: QTextEditH): QTextEditLineWrapMode; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_lineWrapMode';
procedure QTextEdit_setLineWrapMode(handle: QTextEditH; mode: QTextEditLineWrapMode); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setLineWrapMode';
function QTextEdit_lineWrapColumnOrWidth(handle: QTextEditH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_lineWrapColumnOrWidth';
procedure QTextEdit_setLineWrapColumnOrWidth(handle: QTextEditH; w: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setLineWrapColumnOrWidth';
function QTextEdit_wordWrapMode(handle: QTextEditH): QTextOptionWrapMode; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_wordWrapMode';
procedure QTextEdit_setWordWrapMode(handle: QTextEditH; policy: QTextOptionWrapMode); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setWordWrapMode';
function QTextEdit_find(handle: QTextEditH; exp: PWideString; options: QTextDocumentFindFlags = 0): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_find';
procedure QTextEdit_toPlainText(handle: QTextEditH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_toPlainText';
procedure QTextEdit_toHtml(handle: QTextEditH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_toHtml';
procedure QTextEdit_ensureCursorVisible(handle: QTextEditH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_ensureCursorVisible';
procedure QTextEdit_loadResource(handle: QTextEditH; retval: QVariantH; _type: Integer; name: QUrlH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_loadResource';
function QTextEdit_createStandardContextMenu(handle: QTextEditH): QMenuH; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_createStandardContextMenu';
procedure QTextEdit_cursorForPosition(handle: QTextEditH; retval: QTextCursorH; pos: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_cursorForPosition';
procedure QTextEdit_cursorRect(handle: QTextEditH; retval: PRect; cursor: QTextCursorH); overload; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_cursorRect';
procedure QTextEdit_cursorRect(handle: QTextEditH; retval: PRect); overload; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_cursorRect2';
procedure QTextEdit_anchorAt(handle: QTextEditH; retval: PWideString; pos: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_anchorAt';
function QTextEdit_overwriteMode(handle: QTextEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_overwriteMode';
procedure QTextEdit_setOverwriteMode(handle: QTextEditH; overwrite: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setOverwriteMode';
function QTextEdit_tabStopWidth(handle: QTextEditH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_tabStopWidth';
procedure QTextEdit_setTabStopWidth(handle: QTextEditH; width: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setTabStopWidth';
function QTextEdit_acceptRichText(handle: QTextEditH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_acceptRichText';
procedure QTextEdit_setAcceptRichText(handle: QTextEditH; accept: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setAcceptRichText';
procedure QTextEdit_setFontPointSize(handle: QTextEditH; s: Double); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setFontPointSize';
procedure QTextEdit_setFontFamily(handle: QTextEditH; fontFamily: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setFontFamily';
procedure QTextEdit_setFontWeight(handle: QTextEditH; w: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setFontWeight';
procedure QTextEdit_setFontUnderline(handle: QTextEditH; b: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setFontUnderline';
procedure QTextEdit_setFontItalic(handle: QTextEditH; b: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setFontItalic';
procedure QTextEdit_setTextColor(handle: QTextEditH; c: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setTextColor';
procedure QTextEdit_setCurrentFont(handle: QTextEditH; f: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setCurrentFont';
procedure QTextEdit_setAlignment(handle: QTextEditH; a: QtAlignment); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setAlignment';
procedure QTextEdit_setPlainText(handle: QTextEditH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setPlainText';
procedure QTextEdit_setHtml(handle: QTextEditH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_setHtml';
procedure QTextEdit_cut(handle: QTextEditH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_cut';
procedure QTextEdit_copy(handle: QTextEditH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_copy';
procedure QTextEdit_paste(handle: QTextEditH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_paste';
procedure QTextEdit_clear(handle: QTextEditH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_clear';
procedure QTextEdit_selectAll(handle: QTextEditH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_selectAll';
procedure QTextEdit_insertPlainText(handle: QTextEditH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_insertPlainText';
procedure QTextEdit_insertHtml(handle: QTextEditH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_insertHtml';
procedure QTextEdit_append(handle: QTextEditH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_append';
procedure QTextEdit_scrollToAnchor(handle: QTextEditH; name: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_scrollToAnchor';
procedure QTextEdit_zoomIn(handle: QTextEditH; range: Integer = 1); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_zoomIn';
procedure QTextEdit_zoomOut(handle: QTextEditH; range: Integer = 1); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_zoomOut';


type
  QTextEdit_textChanged_Event = procedure () of object cdecl;
  QTextEdit_undoAvailable_Event = procedure (b: Boolean) of object cdecl;
  QTextEdit_redoAvailable_Event = procedure (b: Boolean) of object cdecl;
  QTextEdit_currentCharFormatChanged_Event = procedure (format: QTextCharFormatH) of object cdecl;
  QTextEdit_copyAvailable_Event = procedure (b: Boolean) of object cdecl;
  QTextEdit_selectionChanged_Event = procedure () of object cdecl;
  QTextEdit_cursorPositionChanged_Event = procedure () of object cdecl;


function QMainWindow_create(parent: QWidgetH = nil; flags: QtWindowFlags = 0): QMainWindowH; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_create';
procedure QMainWindow_destroy(handle: QMainWindowH); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_destroy'; 
procedure QMainWindow_iconSize(handle: QMainWindowH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_iconSize';
procedure QMainWindow_setIconSize(handle: QMainWindowH; iconSize: PSize); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_setIconSize';
function QMainWindow_toolButtonStyle(handle: QMainWindowH): QtToolButtonStyle; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_toolButtonStyle';
procedure QMainWindow_setToolButtonStyle(handle: QMainWindowH; toolButtonStyle: QtToolButtonStyle); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_setToolButtonStyle';
function QMainWindow_menuBar(handle: QMainWindowH): QMenuBarH; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_menuBar';
procedure QMainWindow_setMenuBar(handle: QMainWindowH; menubar: QMenuBarH); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_setMenuBar';
function QMainWindow_statusBar(handle: QMainWindowH): QStatusBarH; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_statusBar';
procedure QMainWindow_setStatusBar(handle: QMainWindowH; statusbar: QStatusBarH); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_setStatusBar';
function QMainWindow_centralWidget(handle: QMainWindowH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_centralWidget';
procedure QMainWindow_setCentralWidget(handle: QMainWindowH; widget: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_setCentralWidget';
procedure QMainWindow_setCorner(handle: QMainWindowH; corner: QtCorner; area: QtDockWidgetArea); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_setCorner';
function QMainWindow_corner(handle: QMainWindowH; corner: QtCorner): QtDockWidgetArea; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_corner';
procedure QMainWindow_addToolBarBreak(handle: QMainWindowH; area: QtToolBarArea = QtTopToolBarArea); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_addToolBarBreak';
procedure QMainWindow_insertToolBarBreak(handle: QMainWindowH; before: QToolBarH); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_insertToolBarBreak';
procedure QMainWindow_addToolBar(handle: QMainWindowH; area: QtToolBarArea; toolbar: QToolBarH); overload; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_addToolBar';
procedure QMainWindow_addToolBar(handle: QMainWindowH; toolbar: QToolBarH); overload; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_addToolBar2';
function QMainWindow_addToolBar(handle: QMainWindowH; title: PWideString): QToolBarH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_addToolBar3';
procedure QMainWindow_insertToolBar(handle: QMainWindowH; before: QToolBarH; toolbar: QToolBarH); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_insertToolBar';
procedure QMainWindow_removeToolBar(handle: QMainWindowH; toolbar: QToolBarH); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_removeToolBar';
function QMainWindow_toolBarArea(handle: QMainWindowH; toolbar: QToolBarH): QtToolBarArea; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_toolBarArea';
procedure QMainWindow_addDockWidget(handle: QMainWindowH; area: QtDockWidgetArea; dockwidget: QDockWidgetH); overload; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_addDockWidget';
procedure QMainWindow_addDockWidget(handle: QMainWindowH; area: QtDockWidgetArea; dockwidget: QDockWidgetH; orientation: QtOrientation); overload; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_addDockWidget2';
procedure QMainWindow_splitDockWidget(handle: QMainWindowH; after: QDockWidgetH; dockwidget: QDockWidgetH; orientation: QtOrientation); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_splitDockWidget';
procedure QMainWindow_removeDockWidget(handle: QMainWindowH; dockwidget: QDockWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_removeDockWidget';
function QMainWindow_dockWidgetArea(handle: QMainWindowH; dockwidget: QDockWidgetH): QtDockWidgetArea; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_dockWidgetArea';
procedure QMainWindow_saveState(handle: QMainWindowH; retval: QByteArrayH; version: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_saveState';
function QMainWindow_restoreState(handle: QMainWindowH; state: QByteArrayH; version: Integer = 0): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_restoreState';
function QMainWindow_createPopupMenu(handle: QMainWindowH): QMenuH; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_createPopupMenu';


type
  QMainWindow_iconSizeChanged_Event = procedure (iconSize: PSize) of object cdecl;
  QMainWindow_toolButtonStyleChanged_Event = procedure (toolButtonStyle: QtToolButtonStyle) of object cdecl;


function QToolBar_create(title: PWideString; parent: QWidgetH = nil): QToolBarH; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_create';
procedure QToolBar_destroy(handle: QToolBarH); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_destroy'; 
function QToolBar_create(parent: QWidgetH = nil): QToolBarH; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_create2';
procedure QToolBar_setMovable(handle: QToolBarH; movable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_setMovable';
function QToolBar_isMovable(handle: QToolBarH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_isMovable';
procedure QToolBar_setAllowedAreas(handle: QToolBarH; areas: QtToolBarAreas); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_setAllowedAreas';
function QToolBar_allowedAreas(handle: QToolBarH): QtToolBarAreas; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_allowedAreas';
function QToolBar_isAreaAllowed(handle: QToolBarH; area: QtToolBarArea): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_isAreaAllowed';
procedure QToolBar_setOrientation(handle: QToolBarH; orientation: QtOrientation); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_setOrientation';
function QToolBar_orientation(handle: QToolBarH): QtOrientation; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_orientation';
procedure QToolBar_clear(handle: QToolBarH); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_clear';
function QToolBar_addAction(handle: QToolBarH; text: PWideString): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_addAction';
function QToolBar_addAction(handle: QToolBarH; icon: QIconH; text: PWideString): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_addAction2';
function QToolBar_addAction(handle: QToolBarH; text: PWideString; receiver: QObjectH; member: PAnsiChar): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_addAction3';
function QToolBar_addAction(handle: QToolBarH; icon: QIconH; text: PWideString; receiver: QObjectH; member: PAnsiChar): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_addAction4';
function QToolBar_addSeparator(handle: QToolBarH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_addSeparator';
function QToolBar_insertSeparator(handle: QToolBarH; before: QActionH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_insertSeparator';
function QToolBar_addWidget(handle: QToolBarH; widget: QWidgetH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_addWidget';
function QToolBar_insertWidget(handle: QToolBarH; before: QActionH; widget: QWidgetH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_insertWidget';
procedure QToolBar_actionGeometry(handle: QToolBarH; retval: PRect; action: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_actionGeometry';
function QToolBar_actionAt(handle: QToolBarH; p: PPoint): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_actionAt';
function QToolBar_actionAt(handle: QToolBarH; x: Integer; y: Integer): QActionH; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_actionAt2';
function QToolBar_toggleViewAction(handle: QToolBarH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_toggleViewAction';
procedure QToolBar_iconSize(handle: QToolBarH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_iconSize';
function QToolBar_toolButtonStyle(handle: QToolBarH): QtToolButtonStyle; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_toolButtonStyle';
procedure QToolBar_setIconSize(handle: QToolBarH; iconSize: PSize); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_setIconSize';
procedure QToolBar_setToolButtonStyle(handle: QToolBarH; toolButtonStyle: QtToolButtonStyle); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_setToolButtonStyle';


type
  QToolBar_actionTriggered_Event = procedure (action: QActionH) of object cdecl;
  QToolBar_movableChanged_Event = procedure (movable: Boolean) of object cdecl;
  QToolBar_allowedAreasChanged_Event = procedure (allowedAreas: QtToolBarAreas) of object cdecl;
  QToolBar_orientationChanged_Event = procedure (orientation: QtOrientation) of object cdecl;
  QToolBar_iconSizeChanged_Event = procedure (iconSize: PSize) of object cdecl;
  QToolBar_toolButtonStyleChanged_Event = procedure (toolButtonStyle: QtToolButtonStyle) of object cdecl;


function QSizeGrip_create(parent: QWidgetH): QSizeGripH; cdecl; external QtShareName name QtNamePrefix + 'QSizeGrip_create';
procedure QSizeGrip_destroy(handle: QSizeGripH); cdecl; external QtShareName name QtNamePrefix + 'QSizeGrip_destroy'; 
procedure QSizeGrip_sizeHint(handle: QSizeGripH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QSizeGrip_sizeHint';
procedure QSizeGrip_setVisible(handle: QSizeGripH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QSizeGrip_setVisible';


type
  QLCDNumberMode = ( // QLCDNumber::Mode (1)
    QLCDNumberHex, QLCDNumberDec, QLCDNumberOct, QLCDNumberBin );

  QLCDNumberSegmentStyle = ( // QLCDNumber::SegmentStyle (1)
    QLCDNumberOutline, QLCDNumberFilled, QLCDNumberFlat );

function QLCDNumber_create(parent: QWidgetH = nil): QLCDNumberH; overload; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_create';
procedure QLCDNumber_destroy(handle: QLCDNumberH); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_destroy'; 
function QLCDNumber_create(numDigits: Cardinal; parent: QWidgetH = nil): QLCDNumberH; overload; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_create2';
function QLCDNumber_smallDecimalPoint(handle: QLCDNumberH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_smallDecimalPoint';
function QLCDNumber_numDigits(handle: QLCDNumberH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_numDigits';
procedure QLCDNumber_setNumDigits(handle: QLCDNumberH; nDigits: Integer); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_setNumDigits';
function QLCDNumber_checkOverflow(handle: QLCDNumberH; num: Double): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_checkOverflow';
function QLCDNumber_checkOverflow(handle: QLCDNumberH; num: Integer): Boolean; overload; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_checkOverflow2';
function QLCDNumber_mode(handle: QLCDNumberH): QLCDNumberMode; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_mode';
procedure QLCDNumber_setMode(handle: QLCDNumberH; p1: QLCDNumberMode); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_setMode';
function QLCDNumber_segmentStyle(handle: QLCDNumberH): QLCDNumberSegmentStyle; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_segmentStyle';
procedure QLCDNumber_setSegmentStyle(handle: QLCDNumberH; p1: QLCDNumberSegmentStyle); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_setSegmentStyle';
function QLCDNumber_value(handle: QLCDNumberH): Double; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_value';
function QLCDNumber_intValue(handle: QLCDNumberH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_intValue';
procedure QLCDNumber_sizeHint(handle: QLCDNumberH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_sizeHint';
procedure QLCDNumber_display(handle: QLCDNumberH; str: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_display';
procedure QLCDNumber_display(handle: QLCDNumberH; num: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_display2';
procedure QLCDNumber_display(handle: QLCDNumberH; num: Double); overload; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_display3';
procedure QLCDNumber_setHexMode(handle: QLCDNumberH); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_setHexMode';
procedure QLCDNumber_setDecMode(handle: QLCDNumberH); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_setDecMode';
procedure QLCDNumber_setOctMode(handle: QLCDNumberH); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_setOctMode';
procedure QLCDNumber_setBinMode(handle: QLCDNumberH); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_setBinMode';
procedure QLCDNumber_setSmallDecimalPoint(handle: QLCDNumberH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_setSmallDecimalPoint';


type
  QLCDNumber_overflow_Event = procedure () of object cdecl;



type
  QAbstractSpinBoxButtonSymbols = ( // QAbstractSpinBox::ButtonSymbols (1)
    QAbstractSpinBoxUpDownArrows, QAbstractSpinBoxPlusMinus );

type
  QAbstractSpinBoxStepEnabledFlag = cardinal; // QAbstractSpinBox::StepEnabledFlag
  QAbstractSpinBoxStepEnabled = QAbstractSpinBoxStepEnabledFlag; //QFlags<> (3)
const
  QAbstractSpinBoxStepNone =   $00;
  QAbstractSpinBoxStepUpEnabled =   $01;
  QAbstractSpinBoxStepDownEnabled =   $02;

function QAbstractSpinBox_create(parent: QWidgetH = nil): QAbstractSpinBoxH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_create';
procedure QAbstractSpinBox_destroy(handle: QAbstractSpinBoxH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_destroy'; 
function QAbstractSpinBox_buttonSymbols(handle: QAbstractSpinBoxH): QAbstractSpinBoxButtonSymbols; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_buttonSymbols';
procedure QAbstractSpinBox_setButtonSymbols(handle: QAbstractSpinBoxH; bs: QAbstractSpinBoxButtonSymbols); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_setButtonSymbols';
procedure QAbstractSpinBox_text(handle: QAbstractSpinBoxH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_text';
procedure QAbstractSpinBox_specialValueText(handle: QAbstractSpinBoxH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_specialValueText';
procedure QAbstractSpinBox_setSpecialValueText(handle: QAbstractSpinBoxH; s: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_setSpecialValueText';
function QAbstractSpinBox_wrapping(handle: QAbstractSpinBoxH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_wrapping';
procedure QAbstractSpinBox_setWrapping(handle: QAbstractSpinBoxH; w: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_setWrapping';
procedure QAbstractSpinBox_setReadOnly(handle: QAbstractSpinBoxH; r: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_setReadOnly';
function QAbstractSpinBox_isReadOnly(handle: QAbstractSpinBoxH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_isReadOnly';
procedure QAbstractSpinBox_setAlignment(handle: QAbstractSpinBoxH; flag: QtAlignment); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_setAlignment';
function QAbstractSpinBox_alignment(handle: QAbstractSpinBoxH): QtAlignment; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_alignment';
procedure QAbstractSpinBox_setFrame(handle: QAbstractSpinBoxH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_setFrame';
function QAbstractSpinBox_hasFrame(handle: QAbstractSpinBoxH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_hasFrame';
procedure QAbstractSpinBox_sizeHint(handle: QAbstractSpinBoxH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_sizeHint';
procedure QAbstractSpinBox_minimumSizeHint(handle: QAbstractSpinBoxH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_minimumSizeHint';
procedure QAbstractSpinBox_interpretText(handle: QAbstractSpinBoxH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_interpretText';
function QAbstractSpinBox_event(handle: QAbstractSpinBoxH; event: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_event';
function QAbstractSpinBox_validate(handle: QAbstractSpinBoxH; input: PWideString; pos: PInteger): QValidatorState; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_validate';
procedure QAbstractSpinBox_fixup(handle: QAbstractSpinBoxH; input: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_fixup';
procedure QAbstractSpinBox_stepBy(handle: QAbstractSpinBoxH; steps: Integer); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_stepBy';
procedure QAbstractSpinBox_stepUp(handle: QAbstractSpinBoxH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_stepUp';
procedure QAbstractSpinBox_stepDown(handle: QAbstractSpinBoxH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_stepDown';
procedure QAbstractSpinBox_selectAll(handle: QAbstractSpinBoxH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_selectAll';
procedure QAbstractSpinBox_clear(handle: QAbstractSpinBoxH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_clear';


type
  QAbstractSpinBox_editingFinished_Event = procedure () of object cdecl;


function QSpinBox_create(parent: QWidgetH = nil): QSpinBoxH; cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_create';
procedure QSpinBox_destroy(handle: QSpinBoxH); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_destroy'; 
function QSpinBox_value(handle: QSpinBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_value';
procedure QSpinBox_prefix(handle: QSpinBoxH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_prefix';
procedure QSpinBox_setPrefix(handle: QSpinBoxH; p: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_setPrefix';
procedure QSpinBox_suffix(handle: QSpinBoxH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_suffix';
procedure QSpinBox_setSuffix(handle: QSpinBoxH; s: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_setSuffix';
procedure QSpinBox_cleanText(handle: QSpinBoxH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_cleanText';
function QSpinBox_singleStep(handle: QSpinBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_singleStep';
procedure QSpinBox_setSingleStep(handle: QSpinBoxH; val: Integer); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_setSingleStep';
function QSpinBox_minimum(handle: QSpinBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_minimum';
procedure QSpinBox_setMinimum(handle: QSpinBoxH; min: Integer); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_setMinimum';
function QSpinBox_maximum(handle: QSpinBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_maximum';
procedure QSpinBox_setMaximum(handle: QSpinBoxH; max: Integer); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_setMaximum';
procedure QSpinBox_setRange(handle: QSpinBoxH; min: Integer; max: Integer); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_setRange';
procedure QSpinBox_setValue(handle: QSpinBoxH; val: Integer); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_setValue';

function QDoubleSpinBox_create(parent: QWidgetH = nil): QDoubleSpinBoxH; cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_create';
procedure QDoubleSpinBox_destroy(handle: QDoubleSpinBoxH); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_destroy'; 
function QDoubleSpinBox_value(handle: QDoubleSpinBoxH): Double; cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_value';
procedure QDoubleSpinBox_prefix(handle: QDoubleSpinBoxH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_prefix';
procedure QDoubleSpinBox_setPrefix(handle: QDoubleSpinBoxH; p: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_setPrefix';
procedure QDoubleSpinBox_suffix(handle: QDoubleSpinBoxH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_suffix';
procedure QDoubleSpinBox_setSuffix(handle: QDoubleSpinBoxH; s: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_setSuffix';
procedure QDoubleSpinBox_cleanText(handle: QDoubleSpinBoxH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_cleanText';
function QDoubleSpinBox_singleStep(handle: QDoubleSpinBoxH): Double; cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_singleStep';
procedure QDoubleSpinBox_setSingleStep(handle: QDoubleSpinBoxH; val: Double); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_setSingleStep';
function QDoubleSpinBox_minimum(handle: QDoubleSpinBoxH): Double; cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_minimum';
procedure QDoubleSpinBox_setMinimum(handle: QDoubleSpinBoxH; min: Double); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_setMinimum';
function QDoubleSpinBox_maximum(handle: QDoubleSpinBoxH): Double; cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_maximum';
procedure QDoubleSpinBox_setMaximum(handle: QDoubleSpinBoxH; max: Double); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_setMaximum';
procedure QDoubleSpinBox_setRange(handle: QDoubleSpinBoxH; min: Double; max: Double); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_setRange';
function QDoubleSpinBox_decimals(handle: QDoubleSpinBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_decimals';
procedure QDoubleSpinBox_setDecimals(handle: QDoubleSpinBoxH; prec: Integer); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_setDecimals';
function QDoubleSpinBox_validate(handle: QDoubleSpinBoxH; input: PWideString; pos: PInteger): QValidatorState; cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_validate';
function QDoubleSpinBox_valueFromText(handle: QDoubleSpinBoxH; text: PWideString): Double; cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_valueFromText';
procedure QDoubleSpinBox_textFromValue(handle: QDoubleSpinBoxH; retval: PWideString; v: Double); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_textFromValue';
procedure QDoubleSpinBox_fixup(handle: QDoubleSpinBoxH; str: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_fixup';
procedure QDoubleSpinBox_setValue(handle: QDoubleSpinBoxH; val: Double); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_setValue';


type
  QSpinBox_valueChanged_Event = procedure (p1: Integer) of object cdecl;
  QSpinBox_valueChanged2_Event = procedure (p1: PWideString) of object cdecl;



type
  QDoubleSpinBox_valueChanged_Event = procedure (p1: Double) of object cdecl;
  QDoubleSpinBox_valueChanged2_Event = procedure (p1: PWideString) of object cdecl;


function QSplitter_create(parent: QWidgetH = nil): QSplitterH; overload; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_create';
procedure QSplitter_destroy(handle: QSplitterH); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_destroy'; 
function QSplitter_create(p1: QtOrientation; parent: QWidgetH = nil): QSplitterH; overload; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_create2';
procedure QSplitter_addWidget(handle: QSplitterH; widget: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_addWidget';
procedure QSplitter_insertWidget(handle: QSplitterH; index: Integer; widget: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_insertWidget';
procedure QSplitter_setOrientation(handle: QSplitterH; p1: QtOrientation); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_setOrientation';
function QSplitter_orientation(handle: QSplitterH): QtOrientation; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_orientation';
procedure QSplitter_setChildrenCollapsible(handle: QSplitterH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_setChildrenCollapsible';
function QSplitter_childrenCollapsible(handle: QSplitterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_childrenCollapsible';
procedure QSplitter_setCollapsible(handle: QSplitterH; index: Integer; p2: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_setCollapsible';
function QSplitter_isCollapsible(handle: QSplitterH; index: Integer): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_isCollapsible';
procedure QSplitter_setOpaqueResize(handle: QSplitterH; opaque: Boolean = True); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_setOpaqueResize';
function QSplitter_opaqueResize(handle: QSplitterH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_opaqueResize';
procedure QSplitter_refresh(handle: QSplitterH); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_refresh';
procedure QSplitter_sizeHint(handle: QSplitterH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_sizeHint';
procedure QSplitter_minimumSizeHint(handle: QSplitterH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_minimumSizeHint';
procedure QSplitter_sizes(handle: QSplitterH; retval: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_sizes';
procedure QSplitter_setSizes(handle: QSplitterH; list: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_setSizes';
procedure QSplitter_saveState(handle: QSplitterH; retval: QByteArrayH); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_saveState';
function QSplitter_restoreState(handle: QSplitterH; state: QByteArrayH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_restoreState';
function QSplitter_handleWidth(handle: QSplitterH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_handleWidth';
procedure QSplitter_setHandleWidth(handle: QSplitterH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_setHandleWidth';
function QSplitter_indexOf(handle: QSplitterH; w: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_indexOf';
function QSplitter_widget(handle: QSplitterH; index: Integer): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_widget';
function QSplitter_count(handle: QSplitterH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_count';
procedure QSplitter_getRange(handle: QSplitterH; index: Integer; p2: PInteger; p3: PInteger); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_getRange';
function QSplitter_handle(handle: QSplitterH; index: Integer): QSplitterHandleH; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_handle';
procedure QSplitter_setStretchFactor(handle: QSplitterH; index: Integer; stretch: Integer); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_setStretchFactor';

function QSplitterHandle_create(o: QtOrientation; parent: QSplitterH): QSplitterHandleH; cdecl; external QtShareName name QtNamePrefix + 'QSplitterHandle_create';
procedure QSplitterHandle_destroy(handle: QSplitterHandleH); cdecl; external QtShareName name QtNamePrefix + 'QSplitterHandle_destroy'; 
procedure QSplitterHandle_setOrientation(handle: QSplitterHandleH; o: QtOrientation); cdecl; external QtShareName name QtNamePrefix + 'QSplitterHandle_setOrientation';
function QSplitterHandle_orientation(handle: QSplitterHandleH): QtOrientation; cdecl; external QtShareName name QtNamePrefix + 'QSplitterHandle_orientation';
function QSplitterHandle_opaqueResize(handle: QSplitterHandleH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QSplitterHandle_opaqueResize';
function QSplitterHandle_splitter(handle: QSplitterHandleH): QSplitterH; cdecl; external QtShareName name QtNamePrefix + 'QSplitterHandle_splitter';
procedure QSplitterHandle_sizeHint(handle: QSplitterHandleH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QSplitterHandle_sizeHint';


type
  QSplitter_splitterMoved_Event = procedure (pos: Integer; index: Integer) of object cdecl;



type
  QWorkspaceWindowOrder = ( // QWorkspace::WindowOrder (1)
    QWorkspaceCreationOrder, QWorkspaceStackingOrder );

function QWorkspace_create(parent: QWidgetH = nil): QWorkspaceH; cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_create';
procedure QWorkspace_destroy(handle: QWorkspaceH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_destroy'; 
function QWorkspace_activeWindow(handle: QWorkspaceH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_activeWindow';
procedure QWorkspace_windowList(handle: QWorkspaceH; retval: PIntArray; order: QWorkspaceWindowOrder = QWorkspaceCreationOrder); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_windowList';
function QWorkspace_addWindow(handle: QWorkspaceH; w: QWidgetH; flags: QtWindowFlags = 0): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_addWindow';
procedure QWorkspace_sizeHint(handle: QWorkspaceH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_sizeHint';
function QWorkspace_scrollBarsEnabled(handle: QWorkspaceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_scrollBarsEnabled';
procedure QWorkspace_setScrollBarsEnabled(handle: QWorkspaceH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_setScrollBarsEnabled';
procedure QWorkspace_setBackground(handle: QWorkspaceH; background: QBrushH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_setBackground';
procedure QWorkspace_background(handle: QWorkspaceH; retval: QBrushH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_background';
procedure QWorkspace_setActiveWindow(handle: QWorkspaceH; w: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_setActiveWindow';
procedure QWorkspace_cascade(handle: QWorkspaceH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_cascade';
procedure QWorkspace_tile(handle: QWorkspaceH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_tile';
procedure QWorkspace_arrangeIcons(handle: QWorkspaceH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_arrangeIcons';
procedure QWorkspace_closeActiveWindow(handle: QWorkspaceH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_closeActiveWindow';
procedure QWorkspace_closeAllWindows(handle: QWorkspaceH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_closeAllWindows';
procedure QWorkspace_activateNextWindow(handle: QWorkspaceH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_activateNextWindow';
procedure QWorkspace_activatePreviousWindow(handle: QWorkspaceH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_activatePreviousWindow';


type
  QWorkspace_windowActivated_Event = procedure (w: QWidgetH) of object cdecl;



type
  QComboBoxInsertPolicy = ( // QComboBox::InsertPolicy (1)
    QComboBoxNoInsert, QComboBoxInsertAtTop, QComboBoxInsertAtCurrent, QComboBoxInsertAtBottom, QComboBoxInsertAfterCurrent, QComboBoxInsertBeforeCurrent );

  QComboBoxSizeAdjustPolicy = ( // QComboBox::SizeAdjustPolicy (1)
    QComboBoxAdjustToContents, QComboBoxAdjustToContentsOnFirstShow, QComboBoxAdjustToMinimumContentsLength );

function QComboBox_create(parent: QWidgetH = nil): QComboBoxH; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_create';
procedure QComboBox_destroy(handle: QComboBoxH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_destroy'; 
function QComboBox_maxVisibleItems(handle: QComboBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_maxVisibleItems';
procedure QComboBox_setMaxVisibleItems(handle: QComboBoxH; maxItems: Integer); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setMaxVisibleItems';
function QComboBox_count(handle: QComboBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_count';
procedure QComboBox_setMaxCount(handle: QComboBoxH; max: Integer); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setMaxCount';
function QComboBox_maxCount(handle: QComboBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_maxCount';
function QComboBox_autoCompletion(handle: QComboBoxH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_autoCompletion';
procedure QComboBox_setAutoCompletion(handle: QComboBoxH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setAutoCompletion';
function QComboBox_autoCompletionCaseSensitivity(handle: QComboBoxH): QtCaseSensitivity; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_autoCompletionCaseSensitivity';
procedure QComboBox_setAutoCompletionCaseSensitivity(handle: QComboBoxH; sensitivity: QtCaseSensitivity); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setAutoCompletionCaseSensitivity';
function QComboBox_duplicatesEnabled(handle: QComboBoxH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_duplicatesEnabled';
procedure QComboBox_setDuplicatesEnabled(handle: QComboBoxH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setDuplicatesEnabled';
procedure QComboBox_setFrame(handle: QComboBoxH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setFrame';
function QComboBox_hasFrame(handle: QComboBoxH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_hasFrame';
function QComboBox_findText(handle: QComboBoxH; text: PWideString; flags: QtMatchFlags = QtMatchExactly or QtMatchCaseSensitive): Integer; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_findText';
function QComboBox_findData(handle: QComboBoxH; data: QVariantH; role: Integer = QtUserRole; flags: QtMatchFlags = QtMatchExactly or QtMatchCaseSensitive): Integer; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_findData';
function QComboBox_insertPolicy(handle: QComboBoxH): QComboBoxInsertPolicy; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_insertPolicy';
procedure QComboBox_setInsertPolicy(handle: QComboBoxH; policy: QComboBoxInsertPolicy); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setInsertPolicy';
function QComboBox_sizeAdjustPolicy(handle: QComboBoxH): QComboBoxSizeAdjustPolicy; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_sizeAdjustPolicy';
procedure QComboBox_setSizeAdjustPolicy(handle: QComboBoxH; policy: QComboBoxSizeAdjustPolicy); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setSizeAdjustPolicy';
function QComboBox_minimumContentsLength(handle: QComboBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_minimumContentsLength';
procedure QComboBox_setMinimumContentsLength(handle: QComboBoxH; characters: Integer); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setMinimumContentsLength';
procedure QComboBox_iconSize(handle: QComboBoxH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_iconSize';
procedure QComboBox_setIconSize(handle: QComboBoxH; size: PSize); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setIconSize';
function QComboBox_isEditable(handle: QComboBoxH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_isEditable';
procedure QComboBox_setEditable(handle: QComboBoxH; editable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setEditable';
procedure QComboBox_setLineEdit(handle: QComboBoxH; edit: QLineEditH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setLineEdit';
function QComboBox_lineEdit(handle: QComboBoxH): QLineEditH; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_lineEdit';
procedure QComboBox_setValidator(handle: QComboBoxH; v: QValidatorH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setValidator';
function QComboBox_validator(handle: QComboBoxH): QValidatorH; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_validator';
function QComboBox_itemDelegate(handle: QComboBoxH): QAbstractItemDelegateH; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_itemDelegate';
procedure QComboBox_setItemDelegate(handle: QComboBoxH; delegate: QAbstractItemDelegateH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setItemDelegate';
function QComboBox_model(handle: QComboBoxH): QAbstractItemModelH; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_model';
procedure QComboBox_setModel(handle: QComboBoxH; model: QAbstractItemModelH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setModel';
procedure QComboBox_rootModelIndex(handle: QComboBoxH; retval: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_rootModelIndex';
procedure QComboBox_setRootModelIndex(handle: QComboBoxH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setRootModelIndex';
function QComboBox_modelColumn(handle: QComboBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_modelColumn';
procedure QComboBox_setModelColumn(handle: QComboBoxH; visibleColumn: Integer); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setModelColumn';
function QComboBox_currentIndex(handle: QComboBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_currentIndex';
procedure QComboBox_currentText(handle: QComboBoxH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_currentText';
procedure QComboBox_itemText(handle: QComboBoxH; retval: PWideString; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_itemText';
procedure QComboBox_itemIcon(handle: QComboBoxH; retval: QIconH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_itemIcon';
procedure QComboBox_itemData(handle: QComboBoxH; retval: QVariantH; index: Integer; role: Integer = QtUserRole); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_itemData';
procedure QComboBox_addItem(handle: QComboBoxH; text: PWideString; userData: QVariantH = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_addItem';
procedure QComboBox_addItem(handle: QComboBoxH; icon: QIconH; text: PWideString; userData: QVariantH = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_addItem2';
procedure QComboBox_addItems(handle: QComboBoxH; texts: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_addItems';
procedure QComboBox_insertItem(handle: QComboBoxH; index: Integer; text: PWideString; userData: QVariantH = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_insertItem';
procedure QComboBox_insertItem(handle: QComboBoxH; index: Integer; icon: QIconH; text: PWideString; userData: QVariantH = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_insertItem2';
procedure QComboBox_insertItems(handle: QComboBoxH; index: Integer; texts: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_insertItems';
procedure QComboBox_removeItem(handle: QComboBoxH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_removeItem';
procedure QComboBox_setItemText(handle: QComboBoxH; index: Integer; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setItemText';
procedure QComboBox_setItemIcon(handle: QComboBoxH; index: Integer; icon: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setItemIcon';
procedure QComboBox_setItemData(handle: QComboBoxH; index: Integer; value: QVariantH; role: Integer = QtUserRole); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setItemData';
function QComboBox_view(handle: QComboBoxH): QAbstractItemViewH; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_view';
procedure QComboBox_setView(handle: QComboBoxH; itemView: QAbstractItemViewH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setView';
procedure QComboBox_sizeHint(handle: QComboBoxH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_sizeHint';
procedure QComboBox_minimumSizeHint(handle: QComboBoxH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_minimumSizeHint';
procedure QComboBox_showPopup(handle: QComboBoxH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_showPopup';
procedure QComboBox_hidePopup(handle: QComboBoxH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_hidePopup';
function QComboBox_event(handle: QComboBoxH; event: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_event';
procedure QComboBox_clear(handle: QComboBoxH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_clear';
procedure QComboBox_clearEditText(handle: QComboBoxH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_clearEditText';
procedure QComboBox_setEditText(handle: QComboBoxH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setEditText';
procedure QComboBox_setCurrentIndex(handle: QComboBoxH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_setCurrentIndex';


type
  QComboBox_editTextChanged_Event = procedure (p1: PWideString) of object cdecl;
  QComboBox_activated_Event = procedure (index: Integer) of object cdecl;
  QComboBox_activated2_Event = procedure (p1: PWideString) of object cdecl;
  QComboBox_highlighted_Event = procedure (index: Integer) of object cdecl;
  QComboBox_highlighted2_Event = procedure (p1: PWideString) of object cdecl;
  QComboBox_currentIndexChanged_Event = procedure (index: Integer) of object cdecl;
  QComboBox_currentIndexChanged2_Event = procedure (p1: PWideString) of object cdecl;


function QCheckBox_create(parent: QWidgetH = nil): QCheckBoxH; overload; cdecl; external QtShareName name QtNamePrefix + 'QCheckBox_create';
procedure QCheckBox_destroy(handle: QCheckBoxH); cdecl; external QtShareName name QtNamePrefix + 'QCheckBox_destroy'; 
function QCheckBox_create(text: PWideString; parent: QWidgetH = nil): QCheckBoxH; overload; cdecl; external QtShareName name QtNamePrefix + 'QCheckBox_create2';
procedure QCheckBox_sizeHint(handle: QCheckBoxH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QCheckBox_sizeHint';
procedure QCheckBox_setTristate(handle: QCheckBoxH; y: Boolean = True); cdecl; external QtShareName name QtNamePrefix + 'QCheckBox_setTristate';
function QCheckBox_isTristate(handle: QCheckBoxH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QCheckBox_isTristate';
function QCheckBox_checkState(handle: QCheckBoxH): QtCheckState; cdecl; external QtShareName name QtNamePrefix + 'QCheckBox_checkState';
procedure QCheckBox_setCheckState(handle: QCheckBoxH; state: QtCheckState); cdecl; external QtShareName name QtNamePrefix + 'QCheckBox_setCheckState';


type
  QCheckBox_stateChanged_Event = procedure (p1: Integer) of object cdecl;


type
  QSliderTickPosition = cardinal; //  QSlider::TickPosition (4)

const
    QSliderNoTicks = 0 { $0 };
    QSliderTicksAbove = 1 { $1 };
    QSliderTicksLeft = 1 { $1 };
    QSliderTicksBelow = 2 { $2 };
    QSliderTicksRight = 2 { $2 };
    QSliderTicksBothSides = 3 { $3 };


function QSlider_create(parent: QWidgetH = nil): QSliderH; overload; cdecl; external QtShareName name QtNamePrefix + 'QSlider_create';
procedure QSlider_destroy(handle: QSliderH); cdecl; external QtShareName name QtNamePrefix + 'QSlider_destroy'; 
function QSlider_create(orientation: QtOrientation; parent: QWidgetH = nil): QSliderH; overload; cdecl; external QtShareName name QtNamePrefix + 'QSlider_create2';
procedure QSlider_sizeHint(handle: QSliderH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QSlider_sizeHint';
procedure QSlider_minimumSizeHint(handle: QSliderH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QSlider_minimumSizeHint';
procedure QSlider_setTickPosition(handle: QSliderH; position: QSliderTickPosition); cdecl; external QtShareName name QtNamePrefix + 'QSlider_setTickPosition';
function QSlider_tickPosition(handle: QSliderH): QSliderTickPosition; cdecl; external QtShareName name QtNamePrefix + 'QSlider_tickPosition';
procedure QSlider_setTickInterval(handle: QSliderH; ti: Integer); cdecl; external QtShareName name QtNamePrefix + 'QSlider_setTickInterval';
function QSlider_tickInterval(handle: QSliderH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QSlider_tickInterval';
function QSlider_event(handle: QSliderH; event: QEventH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QSlider_event';

function QTextBrowser_create(parent: QWidgetH = nil): QTextBrowserH; cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_create';
procedure QTextBrowser_destroy(handle: QTextBrowserH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_destroy'; 
procedure QTextBrowser_source(handle: QTextBrowserH; retval: QUrlH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_source';
procedure QTextBrowser_searchPaths(handle: QTextBrowserH; retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_searchPaths';
procedure QTextBrowser_setSearchPaths(handle: QTextBrowserH; paths: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_setSearchPaths';
procedure QTextBrowser_loadResource(handle: QTextBrowserH; retval: QVariantH; _type: Integer; name: QUrlH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_loadResource';
procedure QTextBrowser_setSource(handle: QTextBrowserH; name: QUrlH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_setSource';
procedure QTextBrowser_backward(handle: QTextBrowserH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_backward';
procedure QTextBrowser_forward(handle: QTextBrowserH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_forward';
procedure QTextBrowser_home(handle: QTextBrowserH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_home';
procedure QTextBrowser_reload(handle: QTextBrowserH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_reload';


type
  QTextBrowser_backwardAvailable_Event = procedure (p1: Boolean) of object cdecl;
  QTextBrowser_forwardAvailable_Event = procedure (p1: Boolean) of object cdecl;
  QTextBrowser_sourceChanged_Event = procedure (p1: QUrlH) of object cdecl;
  QTextBrowser_highlighted_Event = procedure (p1: QUrlH) of object cdecl;
  QTextBrowser_highlighted2_Event = procedure (p1: PWideString) of object cdecl;
  QTextBrowser_anchorClicked_Event = procedure (p1: QUrlH) of object cdecl;


function QLabel_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QLabelH; overload; cdecl; external QtShareName name QtNamePrefix + 'QLabel_create';
procedure QLabel_destroy(handle: QLabelH); cdecl; external QtShareName name QtNamePrefix + 'QLabel_destroy'; 
function QLabel_create(text: PWideString; parent: QWidgetH = nil; f: QtWindowFlags = 0): QLabelH; overload; cdecl; external QtShareName name QtNamePrefix + 'QLabel_create2';
procedure QLabel_text(handle: QLabelH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QLabel_text';
function QLabel_pixmap(handle: QLabelH): QPixmapH; cdecl; external QtShareName name QtNamePrefix + 'QLabel_pixmap';
function QLabel_picture(handle: QLabelH): QPictureH; cdecl; external QtShareName name QtNamePrefix + 'QLabel_picture';
function QLabel_movie(handle: QLabelH): QMovieH; cdecl; external QtShareName name QtNamePrefix + 'QLabel_movie';
function QLabel_textFormat(handle: QLabelH): QtTextFormat; cdecl; external QtShareName name QtNamePrefix + 'QLabel_textFormat';
procedure QLabel_setTextFormat(handle: QLabelH; p1: QtTextFormat); cdecl; external QtShareName name QtNamePrefix + 'QLabel_setTextFormat';
function QLabel_alignment(handle: QLabelH): QtAlignment; cdecl; external QtShareName name QtNamePrefix + 'QLabel_alignment';
procedure QLabel_setAlignment(handle: QLabelH; p1: QtAlignment); cdecl; external QtShareName name QtNamePrefix + 'QLabel_setAlignment';
procedure QLabel_setWordWrap(handle: QLabelH; _on: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLabel_setWordWrap';
function QLabel_wordWrap(handle: QLabelH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLabel_wordWrap';
function QLabel_indent(handle: QLabelH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLabel_indent';
procedure QLabel_setIndent(handle: QLabelH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QLabel_setIndent';
function QLabel_margin(handle: QLabelH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLabel_margin';
procedure QLabel_setMargin(handle: QLabelH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QLabel_setMargin';
function QLabel_hasScaledContents(handle: QLabelH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QLabel_hasScaledContents';
procedure QLabel_setScaledContents(handle: QLabelH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QLabel_setScaledContents';
procedure QLabel_sizeHint(handle: QLabelH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLabel_sizeHint';
procedure QLabel_minimumSizeHint(handle: QLabelH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QLabel_minimumSizeHint';
procedure QLabel_setBuddy(handle: QLabelH; p1: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QLabel_setBuddy';
function QLabel_buddy(handle: QLabelH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QLabel_buddy';
function QLabel_heightForWidth(handle: QLabelH; p1: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QLabel_heightForWidth';
procedure QLabel_setText(handle: QLabelH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QLabel_setText';
procedure QLabel_setPixmap(handle: QLabelH; p1: QPixmapH); cdecl; external QtShareName name QtNamePrefix + 'QLabel_setPixmap';
procedure QLabel_setPicture(handle: QLabelH; p1: QPictureH); cdecl; external QtShareName name QtNamePrefix + 'QLabel_setPicture';
procedure QLabel_setMovie(handle: QLabelH; movie: QMovieH); cdecl; external QtShareName name QtNamePrefix + 'QLabel_setMovie';
procedure QLabel_setNum(handle: QLabelH; p1: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QLabel_setNum';
procedure QLabel_setNum(handle: QLabelH; p1: Double); overload; cdecl; external QtShareName name QtNamePrefix + 'QLabel_setNum2';
procedure QLabel_clear(handle: QLabelH); cdecl; external QtShareName name QtNamePrefix + 'QLabel_clear';

function QGroupBox_create(parent: QWidgetH = nil): QGroupBoxH; overload; cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_create';
procedure QGroupBox_destroy(handle: QGroupBoxH); cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_destroy'; 
function QGroupBox_create(title: PWideString; parent: QWidgetH = nil): QGroupBoxH; overload; cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_create2';
procedure QGroupBox_title(handle: QGroupBoxH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_title';
procedure QGroupBox_setTitle(handle: QGroupBoxH; title: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_setTitle';
function QGroupBox_alignment(handle: QGroupBoxH): QtAlignment; cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_alignment';
procedure QGroupBox_setAlignment(handle: QGroupBoxH; alignment: Integer); cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_setAlignment';
procedure QGroupBox_minimumSizeHint(handle: QGroupBoxH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_minimumSizeHint';
function QGroupBox_isFlat(handle: QGroupBoxH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_isFlat';
procedure QGroupBox_setFlat(handle: QGroupBoxH; flat: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_setFlat';
function QGroupBox_isCheckable(handle: QGroupBoxH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_isCheckable';
procedure QGroupBox_setCheckable(handle: QGroupBoxH; checkable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_setCheckable';
function QGroupBox_isChecked(handle: QGroupBoxH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_isChecked';
procedure QGroupBox_setChecked(handle: QGroupBoxH; checked: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_setChecked';


type
  QGroupBox_toggled_Event = procedure (p1: Boolean) of object cdecl;



type
  QTabWidgetTabPosition = ( // QTabWidget::TabPosition (1)
    QTabWidgetNorth, QTabWidgetSouth, QTabWidgetWest, QTabWidgetEast );

  QTabWidgetTabShape = ( // QTabWidget::TabShape (1)
    QTabWidgetRounded, QTabWidgetTriangular );

function QTabWidget_create(parent: QWidgetH = nil): QTabWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_create';
procedure QTabWidget_destroy(handle: QTabWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_destroy'; 
function QTabWidget_addTab(handle: QTabWidgetH; widget: QWidgetH; p2: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_addTab';
function QTabWidget_addTab(handle: QTabWidgetH; widget: QWidgetH; icon: QIconH; _label: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_addTab2';
function QTabWidget_insertTab(handle: QTabWidgetH; index: Integer; widget: QWidgetH; p3: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_insertTab';
function QTabWidget_insertTab(handle: QTabWidgetH; index: Integer; widget: QWidgetH; icon: QIconH; _label: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_insertTab2';
procedure QTabWidget_removeTab(handle: QTabWidgetH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_removeTab';
function QTabWidget_isTabEnabled(handle: QTabWidgetH; index: Integer): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_isTabEnabled';
procedure QTabWidget_setTabEnabled(handle: QTabWidgetH; index: Integer; p2: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_setTabEnabled';
procedure QTabWidget_tabText(handle: QTabWidgetH; retval: PWideString; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_tabText';
procedure QTabWidget_setTabText(handle: QTabWidgetH; index: Integer; p2: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_setTabText';
procedure QTabWidget_tabIcon(handle: QTabWidgetH; retval: QIconH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_tabIcon';
procedure QTabWidget_setTabIcon(handle: QTabWidgetH; index: Integer; icon: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_setTabIcon';
procedure QTabWidget_setTabToolTip(handle: QTabWidgetH; index: Integer; tip: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_setTabToolTip';
procedure QTabWidget_tabToolTip(handle: QTabWidgetH; retval: PWideString; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_tabToolTip';
procedure QTabWidget_setTabWhatsThis(handle: QTabWidgetH; index: Integer; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_setTabWhatsThis';
procedure QTabWidget_tabWhatsThis(handle: QTabWidgetH; retval: PWideString; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_tabWhatsThis';
function QTabWidget_currentIndex(handle: QTabWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_currentIndex';
function QTabWidget_currentWidget(handle: QTabWidgetH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_currentWidget';
function QTabWidget_widget(handle: QTabWidgetH; index: Integer): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_widget';
function QTabWidget_indexOf(handle: QTabWidgetH; widget: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_indexOf';
function QTabWidget_count(handle: QTabWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_count';
function QTabWidget_tabPosition(handle: QTabWidgetH): QTabWidgetTabPosition; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_tabPosition';
procedure QTabWidget_setTabPosition(handle: QTabWidgetH; p1: QTabWidgetTabPosition); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_setTabPosition';
function QTabWidget_tabShape(handle: QTabWidgetH): QTabWidgetTabShape; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_tabShape';
procedure QTabWidget_setTabShape(handle: QTabWidgetH; s: QTabWidgetTabShape); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_setTabShape';
procedure QTabWidget_sizeHint(handle: QTabWidgetH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_sizeHint';
procedure QTabWidget_minimumSizeHint(handle: QTabWidgetH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_minimumSizeHint';
procedure QTabWidget_setCornerWidget(handle: QTabWidgetH; w: QWidgetH; corner: QtCorner = QtTopRightCorner); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_setCornerWidget';
function QTabWidget_cornerWidget(handle: QTabWidgetH; corner: QtCorner = QtTopRightCorner): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_cornerWidget';
procedure QTabWidget_setCurrentIndex(handle: QTabWidgetH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_setCurrentIndex';
procedure QTabWidget_setCurrentWidget(handle: QTabWidgetH; widget: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_setCurrentWidget';


type
  QTabWidget_currentChanged_Event = procedure (index: Integer) of object cdecl;



type
  QTabBarShape = ( // QTabBar::Shape (1)
    QTabBarRoundedNorth, QTabBarRoundedSouth, QTabBarRoundedWest, QTabBarRoundedEast, QTabBarTriangularNorth, QTabBarTriangularSouth, QTabBarTriangularWest, QTabBarTriangularEast );

function QTabBar_create(parent: QWidgetH = nil): QTabBarH; cdecl; external QtShareName name QtNamePrefix + 'QTabBar_create';
procedure QTabBar_destroy(handle: QTabBarH); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_destroy'; 
function QTabBar_shape(handle: QTabBarH): QTabBarShape; cdecl; external QtShareName name QtNamePrefix + 'QTabBar_shape';
procedure QTabBar_setShape(handle: QTabBarH; shape: QTabBarShape); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_setShape';
function QTabBar_addTab(handle: QTabBarH; text: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QTabBar_addTab';
function QTabBar_addTab(handle: QTabBarH; icon: QIconH; text: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QTabBar_addTab2';
function QTabBar_insertTab(handle: QTabBarH; index: Integer; text: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QTabBar_insertTab';
function QTabBar_insertTab(handle: QTabBarH; index: Integer; icon: QIconH; text: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QTabBar_insertTab2';
procedure QTabBar_removeTab(handle: QTabBarH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_removeTab';
function QTabBar_isTabEnabled(handle: QTabBarH; index: Integer): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTabBar_isTabEnabled';
procedure QTabBar_setTabEnabled(handle: QTabBarH; index: Integer; p2: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_setTabEnabled';
procedure QTabBar_tabText(handle: QTabBarH; retval: PWideString; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_tabText';
procedure QTabBar_setTabText(handle: QTabBarH; index: Integer; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_setTabText';
procedure QTabBar_tabTextColor(handle: QTabBarH; retval: PQColor; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_tabTextColor';
procedure QTabBar_setTabTextColor(handle: QTabBarH; index: Integer; color: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_setTabTextColor';
procedure QTabBar_tabIcon(handle: QTabBarH; retval: QIconH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_tabIcon';
procedure QTabBar_setTabIcon(handle: QTabBarH; index: Integer; icon: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_setTabIcon';
procedure QTabBar_setTabToolTip(handle: QTabBarH; index: Integer; tip: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_setTabToolTip';
procedure QTabBar_tabToolTip(handle: QTabBarH; retval: PWideString; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_tabToolTip';
procedure QTabBar_setTabWhatsThis(handle: QTabBarH; index: Integer; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_setTabWhatsThis';
procedure QTabBar_tabWhatsThis(handle: QTabBarH; retval: PWideString; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_tabWhatsThis';
procedure QTabBar_setTabData(handle: QTabBarH; index: Integer; data: QVariantH); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_setTabData';
procedure QTabBar_tabData(handle: QTabBarH; retval: QVariantH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_tabData';
procedure QTabBar_tabRect(handle: QTabBarH; retval: PRect; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_tabRect';
function QTabBar_currentIndex(handle: QTabBarH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabBar_currentIndex';
function QTabBar_count(handle: QTabBarH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTabBar_count';
procedure QTabBar_sizeHint(handle: QTabBarH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_sizeHint';
procedure QTabBar_minimumSizeHint(handle: QTabBarH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_minimumSizeHint';
procedure QTabBar_setDrawBase(handle: QTabBarH; drawTheBase: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_setDrawBase';
function QTabBar_drawBase(handle: QTabBarH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTabBar_drawBase';
procedure QTabBar_iconSize(handle: QTabBarH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_iconSize';
procedure QTabBar_setIconSize(handle: QTabBarH; size: PSize); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_setIconSize';
procedure QTabBar_setCurrentIndex(handle: QTabBarH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_setCurrentIndex';


type
  QTabBar_currentChanged_Event = procedure (index: Integer) of object cdecl;



type
  QProgressBarDirection = ( // QProgressBar::Direction (1)
    QProgressBarTopToBottom, QProgressBarBottomToTop );

function QProgressBar_create(parent: QWidgetH = nil): QProgressBarH; cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_create';
procedure QProgressBar_destroy(handle: QProgressBarH); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_destroy'; 
function QProgressBar_minimum(handle: QProgressBarH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_minimum';
function QProgressBar_maximum(handle: QProgressBarH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_maximum';
procedure QProgressBar_setRange(handle: QProgressBarH; minimum: Integer; maximum: Integer); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_setRange';
function QProgressBar_value(handle: QProgressBarH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_value';
procedure QProgressBar_text(handle: QProgressBarH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_text';
procedure QProgressBar_setTextVisible(handle: QProgressBarH; visible: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_setTextVisible';
function QProgressBar_isTextVisible(handle: QProgressBarH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_isTextVisible';
function QProgressBar_alignment(handle: QProgressBarH): QtAlignment; cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_alignment';
procedure QProgressBar_setAlignment(handle: QProgressBarH; alignment: QtAlignment); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_setAlignment';
procedure QProgressBar_sizeHint(handle: QProgressBarH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_sizeHint';
procedure QProgressBar_minimumSizeHint(handle: QProgressBarH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_minimumSizeHint';
function QProgressBar_orientation(handle: QProgressBarH): QtOrientation; cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_orientation';
procedure QProgressBar_setInvertedAppearance(handle: QProgressBarH; invert: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_setInvertedAppearance';
function QProgressBar_invertedAppearance(handle: QProgressBarH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_invertedAppearance';
procedure QProgressBar_setTextDirection(handle: QProgressBarH; textDirection: QProgressBarDirection); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_setTextDirection';
function QProgressBar_textDirection(handle: QProgressBarH): QProgressBarDirection; cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_textDirection';
procedure QProgressBar_reset(handle: QProgressBarH); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_reset';
procedure QProgressBar_setMinimum(handle: QProgressBarH; minimum: Integer); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_setMinimum';
procedure QProgressBar_setMaximum(handle: QProgressBarH; maximum: Integer); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_setMaximum';
procedure QProgressBar_setValue(handle: QProgressBarH; value: Integer); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_setValue';
procedure QProgressBar_setOrientation(handle: QProgressBarH; p1: QtOrientation); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_setOrientation';


type
  QProgressBar_valueChanged_Event = procedure (value: Integer) of object cdecl;


function QStatusBar_create(parent: QWidgetH = nil): QStatusBarH; cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_create';
procedure QStatusBar_destroy(handle: QStatusBarH); cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_destroy'; 
procedure QStatusBar_addWidget(handle: QStatusBarH; widget: QWidgetH; stretch: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_addWidget';
procedure QStatusBar_addPermanentWidget(handle: QStatusBarH; widget: QWidgetH; stretch: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_addPermanentWidget';
procedure QStatusBar_removeWidget(handle: QStatusBarH; widget: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_removeWidget';
procedure QStatusBar_setSizeGripEnabled(handle: QStatusBarH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_setSizeGripEnabled';
function QStatusBar_isSizeGripEnabled(handle: QStatusBarH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_isSizeGripEnabled';
procedure QStatusBar_currentMessage(handle: QStatusBarH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_currentMessage';
procedure QStatusBar_showMessage(handle: QStatusBarH; text: PWideString; timeout: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_showMessage';
procedure QStatusBar_clearMessage(handle: QStatusBarH); cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_clearMessage';


type
  QStatusBar_messageChanged_Event = procedure (text: PWideString) of object cdecl;


function QToolBox_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QToolBoxH; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_create';
procedure QToolBox_destroy(handle: QToolBoxH); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_destroy'; 
function QToolBox_addItem(handle: QToolBoxH; widget: QWidgetH; text: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_addItem';
function QToolBox_addItem(handle: QToolBoxH; widget: QWidgetH; icon: QIconH; text: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_addItem2';
function QToolBox_insertItem(handle: QToolBoxH; index: Integer; widget: QWidgetH; text: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_insertItem';
function QToolBox_insertItem(handle: QToolBoxH; index: Integer; widget: QWidgetH; icon: QIconH; text: PWideString): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_insertItem2';
procedure QToolBox_removeItem(handle: QToolBoxH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_removeItem';
procedure QToolBox_setItemEnabled(handle: QToolBoxH; index: Integer; enabled: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_setItemEnabled';
function QToolBox_isItemEnabled(handle: QToolBoxH; index: Integer): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_isItemEnabled';
procedure QToolBox_setItemText(handle: QToolBoxH; index: Integer; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_setItemText';
procedure QToolBox_itemText(handle: QToolBoxH; retval: PWideString; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_itemText';
procedure QToolBox_setItemIcon(handle: QToolBoxH; index: Integer; icon: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_setItemIcon';
procedure QToolBox_itemIcon(handle: QToolBoxH; retval: QIconH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_itemIcon';
procedure QToolBox_setItemToolTip(handle: QToolBoxH; index: Integer; toolTip: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_setItemToolTip';
procedure QToolBox_itemToolTip(handle: QToolBoxH; retval: PWideString; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_itemToolTip';
function QToolBox_currentIndex(handle: QToolBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_currentIndex';
function QToolBox_currentWidget(handle: QToolBoxH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_currentWidget';
function QToolBox_widget(handle: QToolBoxH; index: Integer): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_widget';
function QToolBox_indexOf(handle: QToolBoxH; widget: QWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_indexOf';
function QToolBox_count(handle: QToolBoxH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_count';
procedure QToolBox_setCurrentIndex(handle: QToolBoxH; index: Integer); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_setCurrentIndex';
procedure QToolBox_setCurrentWidget(handle: QToolBoxH; widget: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_setCurrentWidget';


type
  QToolBox_currentChanged_Event = procedure (index: Integer) of object cdecl;



type
  QToolButtonToolButtonPopupMode = ( // QToolButton::ToolButtonPopupMode (1)
    QToolButtonDelayedPopup, QToolButtonMenuButtonPopup, QToolButtonInstantPopup );

function QToolButton_create(parent: QWidgetH = nil): QToolButtonH; cdecl; external QtShareName name QtNamePrefix + 'QToolButton_create';
procedure QToolButton_destroy(handle: QToolButtonH); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_destroy'; 
procedure QToolButton_sizeHint(handle: QToolButtonH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_sizeHint';
procedure QToolButton_minimumSizeHint(handle: QToolButtonH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_minimumSizeHint';
function QToolButton_toolButtonStyle(handle: QToolButtonH): QtToolButtonStyle; cdecl; external QtShareName name QtNamePrefix + 'QToolButton_toolButtonStyle';
function QToolButton_arrowType(handle: QToolButtonH): QtArrowType; cdecl; external QtShareName name QtNamePrefix + 'QToolButton_arrowType';
procedure QToolButton_setArrowType(handle: QToolButtonH; _type: QtArrowType); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_setArrowType';
procedure QToolButton_setMenu(handle: QToolButtonH; menu: QMenuH); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_setMenu';
function QToolButton_menu(handle: QToolButtonH): QMenuH; cdecl; external QtShareName name QtNamePrefix + 'QToolButton_menu';
procedure QToolButton_setPopupMode(handle: QToolButtonH; mode: QToolButtonToolButtonPopupMode); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_setPopupMode';
function QToolButton_popupMode(handle: QToolButtonH): QToolButtonToolButtonPopupMode; cdecl; external QtShareName name QtNamePrefix + 'QToolButton_popupMode';
function QToolButton_defaultAction(handle: QToolButtonH): QActionH; cdecl; external QtShareName name QtNamePrefix + 'QToolButton_defaultAction';
procedure QToolButton_setAutoRaise(handle: QToolButtonH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_setAutoRaise';
function QToolButton_autoRaise(handle: QToolButtonH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QToolButton_autoRaise';
procedure QToolButton_showMenu(handle: QToolButtonH); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_showMenu';
procedure QToolButton_setToolButtonStyle(handle: QToolButtonH; style: QtToolButtonStyle); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_setToolButtonStyle';
procedure QToolButton_setDefaultAction(handle: QToolButtonH; p1: QActionH); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_setDefaultAction';


type
  QToolButton_triggered_Event = procedure (p1: QActionH) of object cdecl;



type
  QAbstractItemViewSelectionMode = ( // QAbstractItemView::SelectionMode (1)
    QAbstractItemViewNoSelection, QAbstractItemViewSingleSelection, QAbstractItemViewMultiSelection, QAbstractItemViewExtendedSelection, QAbstractItemViewContiguousSelection );

  QAbstractItemViewSelectionBehavior = ( // QAbstractItemView::SelectionBehavior (1)
    QAbstractItemViewSelectItems, QAbstractItemViewSelectRows, QAbstractItemViewSelectColumns );

  QAbstractItemViewScrollHint = ( // QAbstractItemView::ScrollHint (1)
    QAbstractItemViewEnsureVisible, QAbstractItemViewPositionAtTop, QAbstractItemViewPositionAtBottom );

type
  QAbstractItemViewEditTrigger = cardinal; // QAbstractItemView::EditTrigger
  QAbstractItemViewEditTriggers = QAbstractItemViewEditTrigger; //QFlags<> (3)
const
  QAbstractItemViewNoEditTriggers =   0;
  QAbstractItemViewCurrentChanged =   1;
  QAbstractItemViewDoubleClicked =   2;
  QAbstractItemViewSelectedClicked =   4;
  QAbstractItemViewEditKeyPressed =   8;
  QAbstractItemViewAnyKeyPressed =   16;
  QAbstractItemViewAllEditTriggers =   31;

procedure QAbstractItemView_setModel(handle: QAbstractItemViewH; model: QAbstractItemModelH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setModel';
function QAbstractItemView_model(handle: QAbstractItemViewH): QAbstractItemModelH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_model';
procedure QAbstractItemView_setSelectionModel(handle: QAbstractItemViewH; selectionModel: QItemSelectionModelH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setSelectionModel';
function QAbstractItemView_selectionModel(handle: QAbstractItemViewH): QItemSelectionModelH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_selectionModel';
procedure QAbstractItemView_setItemDelegate(handle: QAbstractItemViewH; delegate: QAbstractItemDelegateH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setItemDelegate';
function QAbstractItemView_itemDelegate(handle: QAbstractItemViewH): QAbstractItemDelegateH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_itemDelegate';
procedure QAbstractItemView_setSelectionMode(handle: QAbstractItemViewH; mode: QAbstractItemViewSelectionMode); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setSelectionMode';
function QAbstractItemView_selectionMode(handle: QAbstractItemViewH): QAbstractItemViewSelectionMode; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_selectionMode';
procedure QAbstractItemView_setSelectionBehavior(handle: QAbstractItemViewH; behavior: QAbstractItemViewSelectionBehavior); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setSelectionBehavior';
function QAbstractItemView_selectionBehavior(handle: QAbstractItemViewH): QAbstractItemViewSelectionBehavior; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_selectionBehavior';
procedure QAbstractItemView_currentIndex(handle: QAbstractItemViewH; retval: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_currentIndex';
procedure QAbstractItemView_rootIndex(handle: QAbstractItemViewH; retval: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_rootIndex';
procedure QAbstractItemView_setEditTriggers(handle: QAbstractItemViewH; triggers: QAbstractItemViewEditTriggers); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setEditTriggers';
function QAbstractItemView_editTriggers(handle: QAbstractItemViewH): QAbstractItemViewEditTriggers; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_editTriggers';
procedure QAbstractItemView_setAutoScroll(handle: QAbstractItemViewH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setAutoScroll';
function QAbstractItemView_hasAutoScroll(handle: QAbstractItemViewH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_hasAutoScroll';
procedure QAbstractItemView_setTabKeyNavigation(handle: QAbstractItemViewH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setTabKeyNavigation';
function QAbstractItemView_tabKeyNavigation(handle: QAbstractItemViewH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_tabKeyNavigation';
procedure QAbstractItemView_setDropIndicatorShown(handle: QAbstractItemViewH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setDropIndicatorShown';
function QAbstractItemView_showDropIndicator(handle: QAbstractItemViewH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_showDropIndicator';
procedure QAbstractItemView_setDragEnabled(handle: QAbstractItemViewH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setDragEnabled';
function QAbstractItemView_dragEnabled(handle: QAbstractItemViewH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_dragEnabled';
procedure QAbstractItemView_setAlternatingRowColors(handle: QAbstractItemViewH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setAlternatingRowColors';
function QAbstractItemView_alternatingRowColors(handle: QAbstractItemViewH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_alternatingRowColors';
procedure QAbstractItemView_setIconSize(handle: QAbstractItemViewH; size: PSize); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setIconSize';
procedure QAbstractItemView_iconSize(handle: QAbstractItemViewH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_iconSize';
procedure QAbstractItemView_setTextElideMode(handle: QAbstractItemViewH; mode: QtTextElideMode); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setTextElideMode';
function QAbstractItemView_textElideMode(handle: QAbstractItemViewH): QtTextElideMode; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_textElideMode';
procedure QAbstractItemView_keyboardSearch(handle: QAbstractItemViewH; search: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_keyboardSearch';
procedure QAbstractItemView_visualRect(handle: QAbstractItemViewH; retval: PRect; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_visualRect';
procedure QAbstractItemView_scrollTo(handle: QAbstractItemViewH; index: QModelIndexH; hint: QAbstractItemViewScrollHint = QAbstractItemViewEnsureVisible); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_scrollTo';
procedure QAbstractItemView_indexAt(handle: QAbstractItemViewH; retval: QModelIndexH; point: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_indexAt';
procedure QAbstractItemView_sizeHintForIndex(handle: QAbstractItemViewH; retval: PSize; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_sizeHintForIndex';
function QAbstractItemView_sizeHintForRow(handle: QAbstractItemViewH; row: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_sizeHintForRow';
function QAbstractItemView_sizeHintForColumn(handle: QAbstractItemViewH; column: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_sizeHintForColumn';
procedure QAbstractItemView_openPersistentEditor(handle: QAbstractItemViewH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_openPersistentEditor';
procedure QAbstractItemView_closePersistentEditor(handle: QAbstractItemViewH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_closePersistentEditor';
procedure QAbstractItemView_setIndexWidget(handle: QAbstractItemViewH; index: QModelIndexH; widget: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setIndexWidget';
function QAbstractItemView_indexWidget(handle: QAbstractItemViewH; index: QModelIndexH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_indexWidget';
procedure QAbstractItemView_reset(handle: QAbstractItemViewH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_reset';
procedure QAbstractItemView_setRootIndex(handle: QAbstractItemViewH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setRootIndex';
procedure QAbstractItemView_doItemsLayout(handle: QAbstractItemViewH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_doItemsLayout';
procedure QAbstractItemView_selectAll(handle: QAbstractItemViewH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_selectAll';
procedure QAbstractItemView_edit(handle: QAbstractItemViewH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_edit';
procedure QAbstractItemView_clearSelection(handle: QAbstractItemViewH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_clearSelection';
procedure QAbstractItemView_setCurrentIndex(handle: QAbstractItemViewH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_setCurrentIndex';
procedure QAbstractItemView_scrollToTop(handle: QAbstractItemViewH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_scrollToTop';
procedure QAbstractItemView_scrollToBottom(handle: QAbstractItemViewH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_scrollToBottom';


type
  QAbstractItemView_pressed_Event = procedure (index: QModelIndexH) of object cdecl;
  QAbstractItemView_clicked_Event = procedure (index: QModelIndexH) of object cdecl;
  QAbstractItemView_doubleClicked_Event = procedure (index: QModelIndexH) of object cdecl;
  QAbstractItemView_activated_Event = procedure (index: QModelIndexH) of object cdecl;
  QAbstractItemView_entered_Event = procedure (index: QModelIndexH) of object cdecl;
  QAbstractItemView_viewportEntered_Event = procedure () of object cdecl;



type
  QListViewMovement = ( // QListView::Movement (1)
    QListViewStatic, QListViewFree, QListViewSnap );

  QListViewFlow = ( // QListView::Flow (1)
    QListViewLeftToRight, QListViewTopToBottom );

  QListViewResizeMode = ( // QListView::ResizeMode (1)
    QListViewFixed, QListViewAdjust );

  QListViewLayoutMode = ( // QListView::LayoutMode (1)
    QListViewSinglePass, QListViewBatched );

  QListViewViewMode = ( // QListView::ViewMode (1)
    QListViewListMode, QListViewIconMode );

function QListView_create(parent: QWidgetH = nil): QListViewH; cdecl; external QtShareName name QtNamePrefix + 'QListView_create';
procedure QListView_destroy(handle: QListViewH); cdecl; external QtShareName name QtNamePrefix + 'QListView_destroy'; 
procedure QListView_setMovement(handle: QListViewH; movement: QListViewMovement); cdecl; external QtShareName name QtNamePrefix + 'QListView_setMovement';
function QListView_movement(handle: QListViewH): QListViewMovement; cdecl; external QtShareName name QtNamePrefix + 'QListView_movement';
procedure QListView_setFlow(handle: QListViewH; flow: QListViewFlow); cdecl; external QtShareName name QtNamePrefix + 'QListView_setFlow';
function QListView_flow(handle: QListViewH): QListViewFlow; cdecl; external QtShareName name QtNamePrefix + 'QListView_flow';
procedure QListView_setWrapping(handle: QListViewH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QListView_setWrapping';
function QListView_isWrapping(handle: QListViewH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QListView_isWrapping';
procedure QListView_setResizeMode(handle: QListViewH; mode: QListViewResizeMode); cdecl; external QtShareName name QtNamePrefix + 'QListView_setResizeMode';
function QListView_resizeMode(handle: QListViewH): QListViewResizeMode; cdecl; external QtShareName name QtNamePrefix + 'QListView_resizeMode';
procedure QListView_setLayoutMode(handle: QListViewH; mode: QListViewLayoutMode); cdecl; external QtShareName name QtNamePrefix + 'QListView_setLayoutMode';
function QListView_layoutMode(handle: QListViewH): QListViewLayoutMode; cdecl; external QtShareName name QtNamePrefix + 'QListView_layoutMode';
procedure QListView_setSpacing(handle: QListViewH; space: Integer); cdecl; external QtShareName name QtNamePrefix + 'QListView_setSpacing';
function QListView_spacing(handle: QListViewH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QListView_spacing';
procedure QListView_setGridSize(handle: QListViewH; size: PSize); cdecl; external QtShareName name QtNamePrefix + 'QListView_setGridSize';
procedure QListView_gridSize(handle: QListViewH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QListView_gridSize';
procedure QListView_setViewMode(handle: QListViewH; mode: QListViewViewMode); cdecl; external QtShareName name QtNamePrefix + 'QListView_setViewMode';
function QListView_viewMode(handle: QListViewH): QListViewViewMode; cdecl; external QtShareName name QtNamePrefix + 'QListView_viewMode';
procedure QListView_clearPropertyFlags(handle: QListViewH); cdecl; external QtShareName name QtNamePrefix + 'QListView_clearPropertyFlags';
function QListView_isRowHidden(handle: QListViewH; row: Integer): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QListView_isRowHidden';
procedure QListView_setRowHidden(handle: QListViewH; row: Integer; hide: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QListView_setRowHidden';
procedure QListView_setModelColumn(handle: QListViewH; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QListView_setModelColumn';
function QListView_modelColumn(handle: QListViewH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QListView_modelColumn';
procedure QListView_setUniformItemSizes(handle: QListViewH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QListView_setUniformItemSizes';
function QListView_uniformItemSizes(handle: QListViewH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QListView_uniformItemSizes';
procedure QListView_visualRect(handle: QListViewH; retval: PRect; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QListView_visualRect';
procedure QListView_scrollTo(handle: QListViewH; index: QModelIndexH; hint: QAbstractItemViewScrollHint); cdecl; external QtShareName name QtNamePrefix + 'QListView_scrollTo';
procedure QListView_indexAt(handle: QListViewH; retval: QModelIndexH; p: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QListView_indexAt';
procedure QListView_doItemsLayout(handle: QListViewH); cdecl; external QtShareName name QtNamePrefix + 'QListView_doItemsLayout';
procedure QListView_reset(handle: QListViewH); cdecl; external QtShareName name QtNamePrefix + 'QListView_reset';
procedure QListView_setRootIndex(handle: QListViewH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QListView_setRootIndex';

type
  QListWidgetItemNoName = cardinal; //  QListWidgetItem::NoName (4)

const
    QListWidgetItemType = 0 { $0 };
    QListWidgetItemUserType = 1000 { $3e8 };


function QListWidgetItem_create(view: QListWidgetH = nil; _type: Integer = QListWidgetItemType): QListWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_create';
procedure QListWidgetItem_destroy(handle: QListWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_destroy'; 
function QListWidgetItem_create(text: PWideString; view: QListWidgetH = nil; _type: Integer = QListWidgetItemType): QListWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_create2';
function QListWidgetItem_create(icon: QIconH; text: PWideString; view: QListWidgetH = nil; _type: Integer = QListWidgetItemType): QListWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_create3';
function QListWidgetItem_create(other: QListWidgetItemH): QListWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_create4';
function QListWidgetItem_clone(handle: QListWidgetItemH): QListWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_clone';
function QListWidgetItem_listWidget(handle: QListWidgetItemH): QListWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_listWidget';
function QListWidgetItem_flags(handle: QListWidgetItemH): QtItemFlags; cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_flags';
procedure QListWidgetItem_setFlags(handle: QListWidgetItemH; flags: QtItemFlags); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setFlags';
procedure QListWidgetItem_text(handle: QListWidgetItemH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_text';
procedure QListWidgetItem_setText(handle: QListWidgetItemH; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setText';
procedure QListWidgetItem_icon(handle: QListWidgetItemH; retval: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_icon';
procedure QListWidgetItem_setIcon(handle: QListWidgetItemH; icon: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setIcon';
procedure QListWidgetItem_statusTip(handle: QListWidgetItemH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_statusTip';
procedure QListWidgetItem_setStatusTip(handle: QListWidgetItemH; statusTip: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setStatusTip';
procedure QListWidgetItem_toolTip(handle: QListWidgetItemH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_toolTip';
procedure QListWidgetItem_setToolTip(handle: QListWidgetItemH; toolTip: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setToolTip';
procedure QListWidgetItem_whatsThis(handle: QListWidgetItemH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_whatsThis';
procedure QListWidgetItem_setWhatsThis(handle: QListWidgetItemH; whatsThis: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setWhatsThis';
procedure QListWidgetItem_font(handle: QListWidgetItemH; retval: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_font';
procedure QListWidgetItem_setFont(handle: QListWidgetItemH; font: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setFont';
function QListWidgetItem_textAlignment(handle: QListWidgetItemH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_textAlignment';
procedure QListWidgetItem_setTextAlignment(handle: QListWidgetItemH; alignment: Integer); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setTextAlignment';
procedure QListWidgetItem_backgroundColor(handle: QListWidgetItemH; retval: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_backgroundColor';
procedure QListWidgetItem_setBackgroundColor(handle: QListWidgetItemH; color: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setBackgroundColor';
procedure QListWidgetItem_textColor(handle: QListWidgetItemH; retval: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_textColor';
procedure QListWidgetItem_setTextColor(handle: QListWidgetItemH; color: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setTextColor';
function QListWidgetItem_checkState(handle: QListWidgetItemH): QtCheckState; cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_checkState';
procedure QListWidgetItem_setCheckState(handle: QListWidgetItemH; state: QtCheckState); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setCheckState';
procedure QListWidgetItem_sizeHint(handle: QListWidgetItemH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_sizeHint';
procedure QListWidgetItem_setSizeHint(handle: QListWidgetItemH; size: PSize); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setSizeHint';
procedure QListWidgetItem_data(handle: QListWidgetItemH; retval: QVariantH; role: Integer); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_data';
procedure QListWidgetItem_setData(handle: QListWidgetItemH; role: Integer; value: QVariantH); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_setData';
procedure QListWidgetItem_read(handle: QListWidgetItemH; _in: QDataStreamH); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_read';
procedure QListWidgetItem_write(handle: QListWidgetItemH; _out: QDataStreamH); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_write';
function QListWidgetItem_type(handle: QListWidgetItemH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_type';

function QListWidget_create(parent: QWidgetH = nil): QListWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_create';
procedure QListWidget_destroy(handle: QListWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_destroy'; 
function QListWidget_item(handle: QListWidgetH; row: Integer): QListWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_item';
function QListWidget_row(handle: QListWidgetH; item: QListWidgetItemH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_row';
procedure QListWidget_insertItem(handle: QListWidgetH; row: Integer; item: QListWidgetItemH); overload; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_insertItem';
procedure QListWidget_insertItem(handle: QListWidgetH; row: Integer; _label: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_insertItem2';
procedure QListWidget_insertItems(handle: QListWidgetH; row: Integer; labels: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_insertItems';
procedure QListWidget_addItem(handle: QListWidgetH; _label: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_addItem';
procedure QListWidget_addItem(handle: QListWidgetH; item: QListWidgetItemH); overload; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_addItem2';
procedure QListWidget_addItems(handle: QListWidgetH; labels: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_addItems';
function QListWidget_takeItem(handle: QListWidgetH; row: Integer): QListWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_takeItem';
function QListWidget_count(handle: QListWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_count';
function QListWidget_currentItem(handle: QListWidgetH): QListWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_currentItem';
procedure QListWidget_setCurrentItem(handle: QListWidgetH; item: QListWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_setCurrentItem';
function QListWidget_currentRow(handle: QListWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_currentRow';
procedure QListWidget_setCurrentRow(handle: QListWidgetH; row: Integer); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_setCurrentRow';
function QListWidget_itemAt(handle: QListWidgetH; p: PPoint): QListWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_itemAt';
function QListWidget_itemAt(handle: QListWidgetH; x: Integer; y: Integer): QListWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_itemAt2';
procedure QListWidget_visualItemRect(handle: QListWidgetH; retval: PRect; item: QListWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_visualItemRect';
procedure QListWidget_sortItems(handle: QListWidgetH; order: QtSortOrder = QtAscendingOrder); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_sortItems';
procedure QListWidget_editItem(handle: QListWidgetH; item: QListWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_editItem';
procedure QListWidget_openPersistentEditor(handle: QListWidgetH; item: QListWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_openPersistentEditor';
procedure QListWidget_closePersistentEditor(handle: QListWidgetH; item: QListWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_closePersistentEditor';
function QListWidget_itemWidget(handle: QListWidgetH; item: QListWidgetItemH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_itemWidget';
procedure QListWidget_setItemWidget(handle: QListWidgetH; item: QListWidgetItemH; widget: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_setItemWidget';
function QListWidget_isItemSelected(handle: QListWidgetH; item: QListWidgetItemH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_isItemSelected';
procedure QListWidget_setItemSelected(handle: QListWidgetH; item: QListWidgetItemH; select: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_setItemSelected';
procedure QListWidget_selectedItems(handle: QListWidgetH; retval: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_selectedItems';
procedure QListWidget_findItems(handle: QListWidgetH; retval: PIntArray; text: PWideString; flags: QtMatchFlags); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_findItems';
function QListWidget_isItemHidden(handle: QListWidgetH; item: QListWidgetItemH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_isItemHidden';
procedure QListWidget_setItemHidden(handle: QListWidgetH; item: QListWidgetItemH; hide: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_setItemHidden';
procedure QListWidget_scrollToItem(handle: QListWidgetH; item: QListWidgetItemH; hint: QAbstractItemViewScrollHint); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_scrollToItem';
procedure QListWidget_clear(handle: QListWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_clear';


type
  QListWidget_itemPressed_Event = procedure (item: QListWidgetItemH) of object cdecl;
  QListWidget_itemClicked_Event = procedure (item: QListWidgetItemH) of object cdecl;
  QListWidget_itemDoubleClicked_Event = procedure (item: QListWidgetItemH) of object cdecl;
  QListWidget_itemActivated_Event = procedure (item: QListWidgetItemH) of object cdecl;
  QListWidget_itemEntered_Event = procedure (item: QListWidgetItemH) of object cdecl;
  QListWidget_itemChanged_Event = procedure (item: QListWidgetItemH) of object cdecl;
  QListWidget_currentItemChanged_Event = procedure (current: QListWidgetItemH; previous: QListWidgetItemH) of object cdecl;
  QListWidget_currentTextChanged_Event = procedure (currentText: PWideString) of object cdecl;
  QListWidget_currentRowChanged_Event = procedure (currentRow: Integer) of object cdecl;
  QListWidget_itemSelectionChanged_Event = procedure () of object cdecl;


function QTreeView_create(parent: QWidgetH = nil): QTreeViewH; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_create';
procedure QTreeView_destroy(handle: QTreeViewH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_destroy'; 
procedure QTreeView_setModel(handle: QTreeViewH; model: QAbstractItemModelH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_setModel';
procedure QTreeView_setRootIndex(handle: QTreeViewH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_setRootIndex';
procedure QTreeView_setSelectionModel(handle: QTreeViewH; selectionModel: QItemSelectionModelH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_setSelectionModel';
function QTreeView_header(handle: QTreeViewH): QHeaderViewH; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_header';
procedure QTreeView_setHeader(handle: QTreeViewH; header: QHeaderViewH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_setHeader';
function QTreeView_indentation(handle: QTreeViewH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_indentation';
procedure QTreeView_setIndentation(handle: QTreeViewH; i: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_setIndentation';
function QTreeView_rootIsDecorated(handle: QTreeViewH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_rootIsDecorated';
procedure QTreeView_setRootIsDecorated(handle: QTreeViewH; show: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_setRootIsDecorated';
function QTreeView_uniformRowHeights(handle: QTreeViewH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_uniformRowHeights';
procedure QTreeView_setUniformRowHeights(handle: QTreeViewH; uniform: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_setUniformRowHeights';
function QTreeView_itemsExpandable(handle: QTreeViewH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_itemsExpandable';
procedure QTreeView_setItemsExpandable(handle: QTreeViewH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_setItemsExpandable';
function QTreeView_columnViewportPosition(handle: QTreeViewH; column: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_columnViewportPosition';
function QTreeView_columnWidth(handle: QTreeViewH; column: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_columnWidth';
function QTreeView_columnAt(handle: QTreeViewH; x: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_columnAt';
function QTreeView_isColumnHidden(handle: QTreeViewH; column: Integer): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_isColumnHidden';
procedure QTreeView_setColumnHidden(handle: QTreeViewH; column: Integer; hide: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_setColumnHidden';
function QTreeView_isRowHidden(handle: QTreeViewH; row: Integer; parent: QModelIndexH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_isRowHidden';
procedure QTreeView_setRowHidden(handle: QTreeViewH; row: Integer; parent: QModelIndexH; hide: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_setRowHidden';
function QTreeView_isExpanded(handle: QTreeViewH; index: QModelIndexH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_isExpanded';
procedure QTreeView_setExpanded(handle: QTreeViewH; index: QModelIndexH; expand: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_setExpanded';
procedure QTreeView_keyboardSearch(handle: QTreeViewH; search: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_keyboardSearch';
procedure QTreeView_visualRect(handle: QTreeViewH; retval: PRect; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_visualRect';
procedure QTreeView_scrollTo(handle: QTreeViewH; index: QModelIndexH; hint: QAbstractItemViewScrollHint); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_scrollTo';
procedure QTreeView_indexAt(handle: QTreeViewH; retval: QModelIndexH; p: PPoint); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_indexAt';
procedure QTreeView_indexAbove(handle: QTreeViewH; retval: QModelIndexH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_indexAbove';
procedure QTreeView_indexBelow(handle: QTreeViewH; retval: QModelIndexH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_indexBelow';
procedure QTreeView_doItemsLayout(handle: QTreeViewH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_doItemsLayout';
procedure QTreeView_reset(handle: QTreeViewH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_reset';
procedure QTreeView_dataChanged(handle: QTreeViewH; topLeft: QModelIndexH; bottomRight: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_dataChanged';
procedure QTreeView_hideColumn(handle: QTreeViewH; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_hideColumn';
procedure QTreeView_showColumn(handle: QTreeViewH; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_showColumn';
procedure QTreeView_expand(handle: QTreeViewH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_expand';
procedure QTreeView_collapse(handle: QTreeViewH; index: QModelIndexH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_collapse';
procedure QTreeView_resizeColumnToContents(handle: QTreeViewH; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_resizeColumnToContents';
procedure QTreeView_sortByColumn(handle: QTreeViewH; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_sortByColumn';
procedure QTreeView_selectAll(handle: QTreeViewH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_selectAll';


type
  QTreeView_expanded_Event = procedure (index: QModelIndexH) of object cdecl;
  QTreeView_collapsed_Event = procedure (index: QModelIndexH) of object cdecl;


function QTreeWidgetItem_create(_type: Integer): QTreeWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_create';
procedure QTreeWidgetItem_destroy(handle: QTreeWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_destroy'; 
function QTreeWidgetItem_create(strings: QStringListH; _type: Integer): QTreeWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_create2';
function QTreeWidgetItem_create(view: QTreeWidgetH; _type: Integer): QTreeWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_create3';
function QTreeWidgetItem_create(view: QTreeWidgetH; strings: QStringListH; _type: Integer): QTreeWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_create4';
function QTreeWidgetItem_create(view: QTreeWidgetH; after: QTreeWidgetItemH; _type: Integer): QTreeWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_create5';
function QTreeWidgetItem_create(parent: QTreeWidgetItemH; _type: Integer): QTreeWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_create6';
function QTreeWidgetItem_create(parent: QTreeWidgetItemH; strings: QStringListH; _type: Integer): QTreeWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_create7';
function QTreeWidgetItem_create(parent: QTreeWidgetItemH; after: QTreeWidgetItemH; _type: Integer): QTreeWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_create8';
function QTreeWidgetItem_create(other: QTreeWidgetItemH): QTreeWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_create9';
function QTreeWidgetItem_clone(handle: QTreeWidgetItemH): QTreeWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_clone';
function QTreeWidgetItem_treeWidget(handle: QTreeWidgetItemH): QTreeWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_treeWidget';
function QTreeWidgetItem_flags(handle: QTreeWidgetItemH): QtItemFlags; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_flags';
procedure QTreeWidgetItem_setFlags(handle: QTreeWidgetItemH; flags: QtItemFlags); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setFlags';
procedure QTreeWidgetItem_text(handle: QTreeWidgetItemH; retval: PWideString; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_text';
procedure QTreeWidgetItem_setText(handle: QTreeWidgetItemH; column: Integer; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setText';
procedure QTreeWidgetItem_icon(handle: QTreeWidgetItemH; retval: QIconH; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_icon';
procedure QTreeWidgetItem_setIcon(handle: QTreeWidgetItemH; column: Integer; icon: QIconH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setIcon';
procedure QTreeWidgetItem_statusTip(handle: QTreeWidgetItemH; retval: PWideString; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_statusTip';
procedure QTreeWidgetItem_setStatusTip(handle: QTreeWidgetItemH; column: Integer; statusTip: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setStatusTip';
procedure QTreeWidgetItem_toolTip(handle: QTreeWidgetItemH; retval: PWideString; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_toolTip';
procedure QTreeWidgetItem_setToolTip(handle: QTreeWidgetItemH; column: Integer; toolTip: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setToolTip';
procedure QTreeWidgetItem_whatsThis(handle: QTreeWidgetItemH; retval: PWideString; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_whatsThis';
procedure QTreeWidgetItem_setWhatsThis(handle: QTreeWidgetItemH; column: Integer; whatsThis: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setWhatsThis';
procedure QTreeWidgetItem_font(handle: QTreeWidgetItemH; retval: QFontH; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_font';
procedure QTreeWidgetItem_setFont(handle: QTreeWidgetItemH; column: Integer; font: QFontH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setFont';
function QTreeWidgetItem_textAlignment(handle: QTreeWidgetItemH; column: Integer): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_textAlignment';
procedure QTreeWidgetItem_setTextAlignment(handle: QTreeWidgetItemH; column: Integer; alignment: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setTextAlignment';
procedure QTreeWidgetItem_backgroundColor(handle: QTreeWidgetItemH; retval: PQColor; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_backgroundColor';
procedure QTreeWidgetItem_setBackgroundColor(handle: QTreeWidgetItemH; column: Integer; color: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setBackgroundColor';
procedure QTreeWidgetItem_textColor(handle: QTreeWidgetItemH; retval: PQColor; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_textColor';
procedure QTreeWidgetItem_setTextColor(handle: QTreeWidgetItemH; column: Integer; color: PQColor); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setTextColor';
function QTreeWidgetItem_checkState(handle: QTreeWidgetItemH; column: Integer): QtCheckState; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_checkState';
procedure QTreeWidgetItem_setCheckState(handle: QTreeWidgetItemH; column: Integer; state: QtCheckState); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setCheckState';
procedure QTreeWidgetItem_sizeHint(handle: QTreeWidgetItemH; retval: PSize; column: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_sizeHint';
procedure QTreeWidgetItem_setSizeHint(handle: QTreeWidgetItemH; column: Integer; size: PSize); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setSizeHint';
procedure QTreeWidgetItem_data(handle: QTreeWidgetItemH; retval: QVariantH; column: Integer; role: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_data';
procedure QTreeWidgetItem_setData(handle: QTreeWidgetItemH; column: Integer; role: Integer; value: QVariantH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_setData';
procedure QTreeWidgetItem_read(handle: QTreeWidgetItemH; _in: QDataStreamH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_read';
procedure QTreeWidgetItem_write(handle: QTreeWidgetItemH; _out: QDataStreamH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_write';
function QTreeWidgetItem_parent(handle: QTreeWidgetItemH): QTreeWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_parent';
function QTreeWidgetItem_child(handle: QTreeWidgetItemH; index: Integer): QTreeWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_child';
function QTreeWidgetItem_childCount(handle: QTreeWidgetItemH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_childCount';
function QTreeWidgetItem_columnCount(handle: QTreeWidgetItemH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_columnCount';
function QTreeWidgetItem_indexOfChild(handle: QTreeWidgetItemH; child: QTreeWidgetItemH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_indexOfChild';
procedure QTreeWidgetItem_addChild(handle: QTreeWidgetItemH; child: QTreeWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_addChild';
procedure QTreeWidgetItem_insertChild(handle: QTreeWidgetItemH; index: Integer; child: QTreeWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_insertChild';
function QTreeWidgetItem_takeChild(handle: QTreeWidgetItemH; index: Integer): QTreeWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_takeChild';
procedure QTreeWidgetItem_addChildren(handle: QTreeWidgetItemH; children: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_addChildren';
procedure QTreeWidgetItem_insertChildren(handle: QTreeWidgetItemH; index: Integer; children: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_insertChildren';
procedure QTreeWidgetItem_takeChildren(handle: QTreeWidgetItemH; retval: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_takeChildren';
function QTreeWidgetItem_type(handle: QTreeWidgetItemH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_type';

function QTreeWidget_create(parent: QWidgetH = nil): QTreeWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_create';
procedure QTreeWidget_destroy(handle: QTreeWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_destroy'; 
function QTreeWidget_columnCount(handle: QTreeWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_columnCount';
procedure QTreeWidget_setColumnCount(handle: QTreeWidgetH; columns: Integer); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_setColumnCount';
function QTreeWidget_topLevelItem(handle: QTreeWidgetH; index: Integer): QTreeWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_topLevelItem';
function QTreeWidget_topLevelItemCount(handle: QTreeWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_topLevelItemCount';
procedure QTreeWidget_insertTopLevelItem(handle: QTreeWidgetH; index: Integer; item: QTreeWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_insertTopLevelItem';
procedure QTreeWidget_addTopLevelItem(handle: QTreeWidgetH; item: QTreeWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_addTopLevelItem';
function QTreeWidget_takeTopLevelItem(handle: QTreeWidgetH; index: Integer): QTreeWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_takeTopLevelItem';
function QTreeWidget_indexOfTopLevelItem(handle: QTreeWidgetH; item: QTreeWidgetItemH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_indexOfTopLevelItem';
procedure QTreeWidget_insertTopLevelItems(handle: QTreeWidgetH; index: Integer; items: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_insertTopLevelItems';
procedure QTreeWidget_addTopLevelItems(handle: QTreeWidgetH; items: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_addTopLevelItems';
function QTreeWidget_headerItem(handle: QTreeWidgetH): QTreeWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_headerItem';
procedure QTreeWidget_setHeaderItem(handle: QTreeWidgetH; item: QTreeWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_setHeaderItem';
procedure QTreeWidget_setHeaderLabels(handle: QTreeWidgetH; labels: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_setHeaderLabels';
function QTreeWidget_currentItem(handle: QTreeWidgetH): QTreeWidgetItemH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_currentItem';
function QTreeWidget_currentColumn(handle: QTreeWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_currentColumn';
procedure QTreeWidget_setCurrentItem(handle: QTreeWidgetH; item: QTreeWidgetItemH); overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_setCurrentItem';
procedure QTreeWidget_setCurrentItem(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer); overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_setCurrentItem2';
function QTreeWidget_itemAt(handle: QTreeWidgetH; p: PPoint): QTreeWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_itemAt';
function QTreeWidget_itemAt(handle: QTreeWidgetH; x: Integer; y: Integer): QTreeWidgetItemH; overload; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_itemAt2';
procedure QTreeWidget_visualItemRect(handle: QTreeWidgetH; retval: PRect; item: QTreeWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_visualItemRect';
function QTreeWidget_sortColumn(handle: QTreeWidgetH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_sortColumn';
procedure QTreeWidget_sortItems(handle: QTreeWidgetH; column: Integer; order: QtSortOrder); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_sortItems';
procedure QTreeWidget_setSortingEnabled(handle: QTreeWidgetH; enable: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_setSortingEnabled';
function QTreeWidget_isSortingEnabled(handle: QTreeWidgetH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_isSortingEnabled';
procedure QTreeWidget_editItem(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_editItem';
procedure QTreeWidget_openPersistentEditor(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_openPersistentEditor';
procedure QTreeWidget_closePersistentEditor(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_closePersistentEditor';
function QTreeWidget_itemWidget(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_itemWidget';
procedure QTreeWidget_setItemWidget(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer; widget: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_setItemWidget';
function QTreeWidget_isItemSelected(handle: QTreeWidgetH; item: QTreeWidgetItemH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_isItemSelected';
procedure QTreeWidget_setItemSelected(handle: QTreeWidgetH; item: QTreeWidgetItemH; select: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_setItemSelected';
procedure QTreeWidget_selectedItems(handle: QTreeWidgetH; retval: PIntArray); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_selectedItems';
procedure QTreeWidget_findItems(handle: QTreeWidgetH; retval: PIntArray; text: PWideString; flags: QtMatchFlags; column: Integer = 0); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_findItems';
function QTreeWidget_isItemHidden(handle: QTreeWidgetH; item: QTreeWidgetItemH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_isItemHidden';
procedure QTreeWidget_setItemHidden(handle: QTreeWidgetH; item: QTreeWidgetItemH; hide: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_setItemHidden';
function QTreeWidget_isItemExpanded(handle: QTreeWidgetH; item: QTreeWidgetItemH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_isItemExpanded';
procedure QTreeWidget_setItemExpanded(handle: QTreeWidgetH; item: QTreeWidgetItemH; expand: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_setItemExpanded';
procedure QTreeWidget_scrollToItem(handle: QTreeWidgetH; item: QTreeWidgetItemH; hint: QAbstractItemViewScrollHint); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_scrollToItem';
procedure QTreeWidget_expandItem(handle: QTreeWidgetH; item: QTreeWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_expandItem';
procedure QTreeWidget_collapseItem(handle: QTreeWidgetH; item: QTreeWidgetItemH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_collapseItem';
procedure QTreeWidget_clear(handle: QTreeWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_clear';


type
  QTreeWidget_itemPressed_Event = procedure (item: QTreeWidgetItemH; column: Integer) of object cdecl;
  QTreeWidget_itemClicked_Event = procedure (item: QTreeWidgetItemH; column: Integer) of object cdecl;
  QTreeWidget_itemDoubleClicked_Event = procedure (item: QTreeWidgetItemH; column: Integer) of object cdecl;
  QTreeWidget_itemActivated_Event = procedure (item: QTreeWidgetItemH; column: Integer) of object cdecl;
  QTreeWidget_itemEntered_Event = procedure (item: QTreeWidgetItemH; column: Integer) of object cdecl;
  QTreeWidget_itemChanged_Event = procedure (item: QTreeWidgetItemH; column: Integer) of object cdecl;
  QTreeWidget_itemExpanded_Event = procedure (item: QTreeWidgetItemH) of object cdecl;
  QTreeWidget_itemCollapsed_Event = procedure (item: QTreeWidgetItemH) of object cdecl;
  QTreeWidget_currentItemChanged_Event = procedure (current: QTreeWidgetItemH; previous: QTreeWidgetItemH) of object cdecl;
  QTreeWidget_itemSelectionChanged_Event = procedure () of object cdecl;



type
  QDialogDialogCode = ( // QDialog::DialogCode (1)
    QDialogRejected, QDialogAccepted );

function QDialog_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QDialogH; cdecl; external QtShareName name QtNamePrefix + 'QDialog_create';
procedure QDialog_destroy(handle: QDialogH); cdecl; external QtShareName name QtNamePrefix + 'QDialog_destroy'; 
function QDialog_result(handle: QDialogH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QDialog_result';
procedure QDialog_setVisible(handle: QDialogH; visible: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QDialog_setVisible';
procedure QDialog_setOrientation(handle: QDialogH; orientation: QtOrientation); cdecl; external QtShareName name QtNamePrefix + 'QDialog_setOrientation';
function QDialog_orientation(handle: QDialogH): QtOrientation; cdecl; external QtShareName name QtNamePrefix + 'QDialog_orientation';
procedure QDialog_setExtension(handle: QDialogH; extension: QWidgetH); cdecl; external QtShareName name QtNamePrefix + 'QDialog_setExtension';
function QDialog_extension(handle: QDialogH): QWidgetH; cdecl; external QtShareName name QtNamePrefix + 'QDialog_extension';
procedure QDialog_sizeHint(handle: QDialogH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QDialog_sizeHint';
procedure QDialog_minimumSizeHint(handle: QDialogH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QDialog_minimumSizeHint';
procedure QDialog_setSizeGripEnabled(handle: QDialogH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QDialog_setSizeGripEnabled';
function QDialog_isSizeGripEnabled(handle: QDialogH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QDialog_isSizeGripEnabled';
procedure QDialog_setModal(handle: QDialogH; modal: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QDialog_setModal';
procedure QDialog_setResult(handle: QDialogH; r: Integer); cdecl; external QtShareName name QtNamePrefix + 'QDialog_setResult';
function QDialog_exec(handle: QDialogH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QDialog_exec';
procedure QDialog_done(handle: QDialogH; p1: Integer); cdecl; external QtShareName name QtNamePrefix + 'QDialog_done';
procedure QDialog_accept(handle: QDialogH); cdecl; external QtShareName name QtNamePrefix + 'QDialog_accept';
procedure QDialog_reject(handle: QDialogH); cdecl; external QtShareName name QtNamePrefix + 'QDialog_reject';
procedure QDialog_showExtension(handle: QDialogH; p1: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QDialog_showExtension';


type
  QDialog_finished_Event = procedure (_result: Integer) of object cdecl;
  QDialog_accepted_Event = procedure () of object cdecl;
  QDialog_rejected_Event = procedure () of object cdecl;


procedure QFontDialog_getFont(retval: QFontH; ok: PBoolean; def: QFontH; parent: QWidgetH = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QFontDialog_getFont';
procedure QFontDialog_getFont(retval: QFontH; ok: PBoolean; parent: QWidgetH = nil); overload; cdecl; external QtShareName name QtNamePrefix + 'QFontDialog_getFont2';


type
  QMessageBoxIcon = (  //QMessageBox::Icon (2s)
    QMessageBoxNoIcon = 0,
    QMessageBoxInformation = 1,
    QMessageBoxWarning = 2,
    QMessageBoxCritical = 3,
    QMessageBoxQuestion = 4 );

  QMessageBoxButton = (  //QMessageBox::Button (2s)
    QMessageBoxNoButton = 0,
    QMessageBoxOk = 1,
    QMessageBoxCancel = 2,
    QMessageBoxYes = 3,
    QMessageBoxNo = 4,
    QMessageBoxAbort = 5,
    QMessageBoxRetry = 6,
    QMessageBoxIgnore = 7,
    QMessageBoxYesAll = 8,
    QMessageBoxNoAll = 9,
    QMessageBoxButtonMask = $ff,
    QMessageBoxDefault = $100,
    QMessageBoxEscape = $200,
    QMessageBoxFlagMask = $300 );

function QMessageBox_create(parent: QWidgetH = nil): QMessageBoxH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_create';
procedure QMessageBox_destroy(handle: QMessageBoxH); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_destroy'; 
function QMessageBox_create(caption: PWideString; text: PWideString; icon: QMessageBoxIcon; button0: Integer; button1: Integer; button2: Integer; parent: QWidgetH = nil; f: QtWindowFlags = QtDialog or QtMSWindowsFixedSizeDialogHint): QMessageBoxH; overload; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_create2';
procedure QMessageBox_text(handle: QMessageBoxH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_text';
procedure QMessageBox_setText(handle: QMessageBoxH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_setText';
function QMessageBox_icon(handle: QMessageBoxH): QMessageBoxIcon; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_icon';
procedure QMessageBox_setIcon(handle: QMessageBoxH; p1: QMessageBoxIcon); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_setIcon';
procedure QMessageBox_iconPixmap(handle: QMessageBoxH; retval: QPixmapH); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_iconPixmap';
procedure QMessageBox_setIconPixmap(handle: QMessageBoxH; p1: QPixmapH); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_setIconPixmap';
procedure QMessageBox_buttonText(handle: QMessageBoxH; retval: PWideString; button: Integer); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_buttonText';
procedure QMessageBox_setButtonText(handle: QMessageBoxH; button: Integer; p2: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_setButtonText';
function QMessageBox_textFormat(handle: QMessageBoxH): QtTextFormat; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_textFormat';
procedure QMessageBox_setTextFormat(handle: QMessageBoxH; p1: QtTextFormat); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_setTextFormat';
function QMessageBox_information(parent: QWidgetH; caption: PWideString; text: PWideString; button0: Integer; button1: Integer = 0; button2: Integer = 0): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_information';
function QMessageBox_information(parent: QWidgetH; caption: PWideString; text: PWideString; button0Text: PWideString = nil; button1Text: PWideString = nil; button2Text: PWideString = nil; defaultButtonNumber: Integer = 0; escapeButtonNumber: Integer = -1): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_information2';
function QMessageBox_question(parent: QWidgetH; caption: PWideString; text: PWideString; button0: Integer; button1: Integer = 0; button2: Integer = 0): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_question';
function QMessageBox_question(parent: QWidgetH; caption: PWideString; text: PWideString; button0Text: PWideString = nil; button1Text: PWideString = nil; button2Text: PWideString = nil; defaultButtonNumber: Integer = 0; escapeButtonNumber: Integer = -1): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_question2';
function QMessageBox_warning(parent: QWidgetH; caption: PWideString; text: PWideString; button0: Integer; button1: Integer; button2: Integer = 0): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_warning';
function QMessageBox_warning(parent: QWidgetH; caption: PWideString; text: PWideString; button0Text: PWideString = nil; button1Text: PWideString = nil; button2Text: PWideString = nil; defaultButtonNumber: Integer = 0; escapeButtonNumber: Integer = -1): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_warning2';
function QMessageBox_critical(parent: QWidgetH; caption: PWideString; text: PWideString; button0: Integer; button1: Integer; button2: Integer = 0): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_critical';
function QMessageBox_critical(parent: QWidgetH; caption: PWideString; text: PWideString; button0Text: PWideString = nil; button1Text: PWideString = nil; button2Text: PWideString = nil; defaultButtonNumber: Integer = 0; escapeButtonNumber: Integer = -1): Integer; overload; cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_critical2';
procedure QMessageBox_about(parent: QWidgetH; caption: PWideString; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_about';
procedure QMessageBox_aboutQt(parent: QWidgetH; caption: PWideString = nil); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_aboutQt';
procedure QMessageBox_sizeHint(handle: QMessageBoxH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_sizeHint';
procedure QMessageBox_standardIcon(retval: QPixmapH; icon: QMessageBoxIcon); cdecl; external QtShareName name QtNamePrefix + 'QMessageBox_standardIcon';

procedure QInputDialog_getText(retval: PWideString; parent: QWidgetH; title: PWideString; _label: PWideString; echo: QLineEditEchoMode = QLineEditNormal; text: PWideString = nil; ok: PBoolean = 0; f: QtWindowFlags = 0); cdecl; external QtShareName name QtNamePrefix + 'QInputDialog_getText';
function QInputDialog_getInteger(parent: QWidgetH; title: PWideString; _label: PWideString; value: Integer = 0; minValue: Integer = -2147483647; maxValue: Integer = 2147483647; step: Integer = 1; ok: PBoolean = 0; f: QtWindowFlags = 0): Integer; cdecl; external QtShareName name QtNamePrefix + 'QInputDialog_getInteger';
function QInputDialog_getDouble(parent: QWidgetH; title: PWideString; _label: PWideString; value: Double = 0; minValue: Double = -2147483647; maxValue: Double = 2147483647; decimals: Integer = 1; ok: PBoolean = 0; f: QtWindowFlags = 0): Double; cdecl; external QtShareName name QtNamePrefix + 'QInputDialog_getDouble';
procedure QInputDialog_getItem(retval: PWideString; parent: QWidgetH; title: PWideString; _label: PWideString; list: QStringListH; current: Integer = 0; editable: Boolean = True; ok: PBoolean = 0; f: QtWindowFlags = 0); cdecl; external QtShareName name QtNamePrefix + 'QInputDialog_getItem';

procedure QColorDialog_getColor(retval: PQColor; init: PQColor = Qtwhite; parent: QWidgetH = nil); cdecl; external QtShareName name QtNamePrefix + 'QColorDialog_getColor';
function QColorDialog_getRgba(p1: QRgb; ok: PBoolean = 0; parent: QWidgetH = nil): QRgb; cdecl; external QtShareName name QtNamePrefix + 'QColorDialog_getRgba';
function QColorDialog_customCount(): Integer; cdecl; external QtShareName name QtNamePrefix + 'QColorDialog_customCount';
function QColorDialog_customColor(p1: Integer): QRgb; cdecl; external QtShareName name QtNamePrefix + 'QColorDialog_customColor';
procedure QColorDialog_setCustomColor(p1: Integer; p2: QRgb); cdecl; external QtShareName name QtNamePrefix + 'QColorDialog_setCustomColor';
procedure QColorDialog_setStandardColor(p1: Integer; p2: QRgb); cdecl; external QtShareName name QtNamePrefix + 'QColorDialog_setStandardColor';


type
  QFileDialogViewMode = ( // QFileDialog::ViewMode (1)
    QFileDialogDetail, QFileDialogList );

  QFileDialogFileMode = ( // QFileDialog::FileMode (1)
    QFileDialogAnyFile, QFileDialogExistingFile, QFileDialogDirectory, QFileDialogExistingFiles, QFileDialogDirectoryOnly );

  QFileDialogAcceptMode = ( // QFileDialog::AcceptMode (1)
    QFileDialogAcceptOpen, QFileDialogAcceptSave );

  QFileDialogDialogLabel = ( // QFileDialog::DialogLabel (1)
    QFileDialogLookIn, QFileDialogFileName, QFileDialogFileType, QFileDialogAccept, QFileDialogReject );

type
  QFileDialogOption = cardinal; // QFileDialog::Option
  QFileDialogOptions = QFileDialogOption; //QFlags<> (3)
const
  QFileDialogShowDirsOnly =   $01;
  QFileDialogDontResolveSymlinks =   $02;
  QFileDialogDontConfirmOverwrite =   $04;
  QFileDialogDontUseSheet =   $08;
  QFileDialogDontUseNativeDialog =   $10;

function QFileDialog_create(parent: QWidgetH; f: QtWindowFlags): QFileDialogH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_create';
procedure QFileDialog_destroy(handle: QFileDialogH); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_destroy'; 
function QFileDialog_create(parent: QWidgetH = nil; caption: PWideString = nil; directory: PWideString = nil; filter: PWideString = nil): QFileDialogH; overload; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_create2';
procedure QFileDialog_setDirectory(handle: QFileDialogH; directory: PWideString); overload; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setDirectory';
procedure QFileDialog_setDirectory(handle: QFileDialogH; directory: QDirH); overload; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setDirectory2';
procedure QFileDialog_directory(handle: QFileDialogH; retval: QDirH); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_directory';
procedure QFileDialog_selectFile(handle: QFileDialogH; filename: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_selectFile';
procedure QFileDialog_selectedFiles(handle: QFileDialogH; retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_selectedFiles';
procedure QFileDialog_setFilter(handle: QFileDialogH; filter: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setFilter';
procedure QFileDialog_setFilters(handle: QFileDialogH; filters: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setFilters';
procedure QFileDialog_filters(handle: QFileDialogH; retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_filters';
procedure QFileDialog_selectFilter(handle: QFileDialogH; filter: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_selectFilter';
procedure QFileDialog_selectedFilter(handle: QFileDialogH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_selectedFilter';
procedure QFileDialog_setViewMode(handle: QFileDialogH; mode: QFileDialogViewMode); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setViewMode';
function QFileDialog_viewMode(handle: QFileDialogH): QFileDialogViewMode; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_viewMode';
procedure QFileDialog_setFileMode(handle: QFileDialogH; mode: QFileDialogFileMode); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setFileMode';
function QFileDialog_fileMode(handle: QFileDialogH): QFileDialogFileMode; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_fileMode';
procedure QFileDialog_setAcceptMode(handle: QFileDialogH; mode: QFileDialogAcceptMode); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setAcceptMode';
function QFileDialog_acceptMode(handle: QFileDialogH): QFileDialogAcceptMode; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_acceptMode';
procedure QFileDialog_setReadOnly(handle: QFileDialogH; enabled: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setReadOnly';
function QFileDialog_isReadOnly(handle: QFileDialogH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_isReadOnly';
procedure QFileDialog_setResolveSymlinks(handle: QFileDialogH; enabled: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setResolveSymlinks';
function QFileDialog_resolveSymlinks(handle: QFileDialogH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_resolveSymlinks';
procedure QFileDialog_setConfirmOverwrite(handle: QFileDialogH; enabled: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setConfirmOverwrite';
function QFileDialog_confirmOverwrite(handle: QFileDialogH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_confirmOverwrite';
procedure QFileDialog_setDefaultSuffix(handle: QFileDialogH; suffix: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setDefaultSuffix';
procedure QFileDialog_defaultSuffix(handle: QFileDialogH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_defaultSuffix';
procedure QFileDialog_setHistory(handle: QFileDialogH; paths: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setHistory';
procedure QFileDialog_history(handle: QFileDialogH; retval: QStringListH); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_history';
procedure QFileDialog_setItemDelegate(handle: QFileDialogH; delegate: QAbstractItemDelegateH); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setItemDelegate';
function QFileDialog_itemDelegate(handle: QFileDialogH): QAbstractItemDelegateH; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_itemDelegate';
procedure QFileDialog_setIconProvider(handle: QFileDialogH; provider: QFileIconProviderH); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setIconProvider';
function QFileDialog_iconProvider(handle: QFileDialogH): QFileIconProviderH; cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_iconProvider';
procedure QFileDialog_setLabelText(handle: QFileDialogH; _label: QFileDialogDialogLabel; text: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_setLabelText';
procedure QFileDialog_labelText(handle: QFileDialogH; retval: PWideString; _label: QFileDialogDialogLabel); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_labelText';
procedure QFileDialog_getOpenFileName(retval: PWideString; parent: QWidgetH = nil; caption: PWideString = nil; dir: PWideString = nil; filter: PWideString = nil; selectedFilter: PWideString = nil; options: QFileDialogOptions = 0); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_getOpenFileName';
procedure QFileDialog_getSaveFileName(retval: PWideString; parent: QWidgetH = nil; caption: PWideString = nil; dir: PWideString = nil; filter: PWideString = nil; selectedFilter: PWideString = nil; options: QFileDialogOptions = 0); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_getSaveFileName';
procedure QFileDialog_getExistingDirectory(retval: PWideString; parent: QWidgetH = nil; caption: PWideString = nil; dir: PWideString = nil; options: QFileDialogOptions = QFileDialogShowDirsOnly); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_getExistingDirectory';
procedure QFileDialog_getOpenFileNames(retval: QStringListH; parent: QWidgetH = nil; caption: PWideString = nil; dir: PWideString = nil; filter: PWideString = nil; selectedFilter: PWideString = nil; options: QFileDialogOptions = 0); cdecl; external QtShareName name QtNamePrefix + 'QFileDialog_getOpenFileNames';

function QProgressDialog_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QProgressDialogH; overload; cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_create';
procedure QProgressDialog_destroy(handle: QProgressDialogH); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_destroy'; 
function QProgressDialog_create(labelText: PWideString; cancelButtonText: PWideString; minimum: Integer; maximum: Integer; parent: QWidgetH = nil; f: QtWindowFlags = 0): QProgressDialogH; overload; cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_create2';
procedure QProgressDialog_setLabel(handle: QProgressDialogH; _label: QLabelH); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setLabel';
procedure QProgressDialog_setCancelButton(handle: QProgressDialogH; button: QPushButtonH); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setCancelButton';
procedure QProgressDialog_setBar(handle: QProgressDialogH; bar: QProgressBarH); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setBar';
function QProgressDialog_wasCanceled(handle: QProgressDialogH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_wasCanceled';
function QProgressDialog_minimum(handle: QProgressDialogH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_minimum';
function QProgressDialog_maximum(handle: QProgressDialogH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_maximum';
procedure QProgressDialog_setRange(handle: QProgressDialogH; minimum: Integer; maximum: Integer); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setRange';
function QProgressDialog_value(handle: QProgressDialogH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_value';
procedure QProgressDialog_sizeHint(handle: QProgressDialogH; retval: PSize); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_sizeHint';
procedure QProgressDialog_labelText(handle: QProgressDialogH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_labelText';
function QProgressDialog_minimumDuration(handle: QProgressDialogH): Integer; cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_minimumDuration';
procedure QProgressDialog_setAutoReset(handle: QProgressDialogH; b: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setAutoReset';
function QProgressDialog_autoReset(handle: QProgressDialogH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_autoReset';
procedure QProgressDialog_setAutoClose(handle: QProgressDialogH; b: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setAutoClose';
function QProgressDialog_autoClose(handle: QProgressDialogH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_autoClose';
procedure QProgressDialog_cancel(handle: QProgressDialogH); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_cancel';
procedure QProgressDialog_reset(handle: QProgressDialogH); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_reset';
procedure QProgressDialog_setMaximum(handle: QProgressDialogH; maximum: Integer); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setMaximum';
procedure QProgressDialog_setMinimum(handle: QProgressDialogH; minimum: Integer); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setMinimum';
procedure QProgressDialog_setValue(handle: QProgressDialogH; progress: Integer); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setValue';
procedure QProgressDialog_setLabelText(handle: QProgressDialogH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setLabelText';
procedure QProgressDialog_setCancelButtonText(handle: QProgressDialogH; p1: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setCancelButtonText';
procedure QProgressDialog_setMinimumDuration(handle: QProgressDialogH; ms: Integer); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_setMinimumDuration';


type
  QProgressDialog_canceled_Event = procedure () of object cdecl;


type
  QIODeviceOpenModeFlag = cardinal; //  QIODevice::OpenModeFlag (4)
  QIODeviceOpenMode = QIODeviceOpenModeFlag; // QFlags<>

const
    QIODeviceNotOpen = 0 { $0 };
    QIODeviceReadOnly = 1 { $1 };
    QIODeviceWriteOnly = 2 { $2 };
    QIODeviceReadWrite = 3 { $3 };
    QIODeviceAppend = 4 { $4 };
    QIODeviceTruncate = 8 { $8 };
    QIODeviceText = 16 { $10 };
    QIODeviceUnbuffered = 32 { $20 };


function QIODevice_openMode(handle: QIODeviceH): QIODeviceOpenMode; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_openMode';
procedure QIODevice_setTextModeEnabled(handle: QIODeviceH; enabled: Boolean); cdecl; external QtShareName name QtNamePrefix + 'QIODevice_setTextModeEnabled';
function QIODevice_isTextModeEnabled(handle: QIODeviceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_isTextModeEnabled';
function QIODevice_isOpen(handle: QIODeviceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_isOpen';
function QIODevice_isReadable(handle: QIODeviceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_isReadable';
function QIODevice_isWritable(handle: QIODeviceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_isWritable';
function QIODevice_isSequential(handle: QIODeviceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_isSequential';
function QIODevice_open(handle: QIODeviceH; mode: QIODeviceOpenMode): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_open';
procedure QIODevice_close(handle: QIODeviceH); cdecl; external QtShareName name QtNamePrefix + 'QIODevice_close';
function QIODevice_pos(handle: QIODeviceH): int64; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_pos';
function QIODevice_size(handle: QIODeviceH): int64; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_size';
function QIODevice_seek(handle: QIODeviceH; pos: int64): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_seek';
function QIODevice_atEnd(handle: QIODeviceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_atEnd';
function QIODevice_reset(handle: QIODeviceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_reset';
function QIODevice_bytesAvailable(handle: QIODeviceH): int64; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_bytesAvailable';
function QIODevice_bytesToWrite(handle: QIODeviceH): int64; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_bytesToWrite';
function QIODevice_read(handle: QIODeviceH; data: PAnsiChar; maxlen: int64): int64; overload; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_read';
procedure QIODevice_read(handle: QIODeviceH; retval: QByteArrayH; maxlen: int64); overload; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_read2';
procedure QIODevice_readAll(handle: QIODeviceH; retval: QByteArrayH); cdecl; external QtShareName name QtNamePrefix + 'QIODevice_readAll';
function QIODevice_readLine(handle: QIODeviceH; data: PAnsiChar; maxlen: int64): int64; overload; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_readLine';
procedure QIODevice_readLine(handle: QIODeviceH; retval: QByteArrayH; maxlen: int64 = 0); overload; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_readLine2';
function QIODevice_canReadLine(handle: QIODeviceH): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_canReadLine';
function QIODevice_write(handle: QIODeviceH; data: PAnsiChar; len: int64): int64; overload; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_write';
function QIODevice_write(handle: QIODeviceH; data: QByteArrayH): int64; overload; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_write2';
function QIODevice_peek(handle: QIODeviceH; data: PAnsiChar; maxlen: int64): int64; overload; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_peek';
procedure QIODevice_peek(handle: QIODeviceH; retval: QByteArrayH; maxlen: int64); overload; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_peek2';
function QIODevice_waitForReadyRead(handle: QIODeviceH; msecs: Integer): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_waitForReadyRead';
function QIODevice_waitForBytesWritten(handle: QIODeviceH; msecs: Integer): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_waitForBytesWritten';
procedure QIODevice_ungetChar(handle: QIODeviceH; c: char); cdecl; external QtShareName name QtNamePrefix + 'QIODevice_ungetChar';
function QIODevice_putChar(handle: QIODeviceH; c: char): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_putChar';
function QIODevice_getChar(handle: QIODeviceH; c: PAnsiChar): Boolean; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_getChar';
procedure QIODevice_errorString(handle: QIODeviceH; retval: PWideString); cdecl; external QtShareName name QtNamePrefix + 'QIODevice_errorString';


type
  QIODevice_readyRead_Event = procedure () of object cdecl;
  QIODevice_bytesWritten_Event = procedure (bytes: int64) of object cdecl;
  QIODevice_aboutToClose_Event = procedure () of object cdecl;


function QEvent_hook_create(handle: QObjectH): QEvent_hookH; cdecl; external QtShareName name QtNamePrefix + 'QEvent_hook_create';
procedure QEvent_hook_destroy(handle: QEvent_hookH); cdecl; external QtShareName name QtNamePrefix + 'QEvent_hook_destroy'; 

function QTimerEvent_hook_create(handle: QObjectH): QTimerEvent_hookH; cdecl; external QtShareName name QtNamePrefix + 'QTimerEvent_hook_create';
procedure QTimerEvent_hook_destroy(handle: QTimerEvent_hookH); cdecl; external QtShareName name QtNamePrefix + 'QTimerEvent_hook_destroy'; 

function QChildEvent_hook_create(handle: QObjectH): QChildEvent_hookH; cdecl; external QtShareName name QtNamePrefix + 'QChildEvent_hook_create';
procedure QChildEvent_hook_destroy(handle: QChildEvent_hookH); cdecl; external QtShareName name QtNamePrefix + 'QChildEvent_hook_destroy'; 

function QEventLoop_hook_create(handle: QObjectH): QEventLoop_hookH; cdecl; external QtShareName name QtNamePrefix + 'QEventLoop_hook_create';
procedure QEventLoop_hook_destroy(handle: QEventLoop_hookH); cdecl; external QtShareName name QtNamePrefix + 'QEventLoop_hook_destroy'; 

function QCoreApplication_hook_create(handle: QObjectH): QCoreApplication_hookH; cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_hook_create';
procedure QCoreApplication_hook_destroy(handle: QCoreApplication_hookH); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_hook_destroy'; 
procedure QCoreApplication_hook_hook_aboutToQuit(handle: QCoreApplication_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_hook_hook_aboutToQuit';
procedure QCoreApplication_hook_hook_unixSignal(handle: QCoreApplication_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QCoreApplication_hook_hook_unixSignal';

function QTimer_hook_create(handle: QObjectH): QTimer_hookH; cdecl; external QtShareName name QtNamePrefix + 'QTimer_hook_create';
procedure QTimer_hook_destroy(handle: QTimer_hookH); cdecl; external QtShareName name QtNamePrefix + 'QTimer_hook_destroy'; 
procedure QTimer_hook_hook_timeout(handle: QTimer_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTimer_hook_hook_timeout';

function QApplication_hook_create(handle: QObjectH): QApplication_hookH; cdecl; external QtShareName name QtNamePrefix + 'QApplication_hook_create';
procedure QApplication_hook_destroy(handle: QApplication_hookH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_hook_destroy'; 
procedure QApplication_hook_hook_lastWindowClosed(handle: QApplication_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_hook_hook_lastWindowClosed';
procedure QApplication_hook_hook_focusChanged(handle: QApplication_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QApplication_hook_hook_focusChanged';

function QWidget_hook_create(handle: QObjectH): QWidget_hookH; cdecl; external QtShareName name QtNamePrefix + 'QWidget_hook_create';
procedure QWidget_hook_destroy(handle: QWidget_hookH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_hook_destroy'; 
procedure QWidget_hook_hook_customContextMenuRequested(handle: QWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QWidget_hook_hook_customContextMenuRequested';

function QAction_hook_create(handle: QObjectH): QAction_hookH; cdecl; external QtShareName name QtNamePrefix + 'QAction_hook_create';
procedure QAction_hook_destroy(handle: QAction_hookH); cdecl; external QtShareName name QtNamePrefix + 'QAction_hook_destroy'; 
procedure QAction_hook_hook_changed(handle: QAction_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAction_hook_hook_changed';
procedure QAction_hook_hook_triggered(handle: QAction_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAction_hook_hook_triggered';
procedure QAction_hook_hook_triggered2(handle: QAction_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAction_hook_hook_triggered2';
procedure QAction_hook_hook_hovered(handle: QAction_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAction_hook_hook_hovered';
procedure QAction_hook_hook_toggled(handle: QAction_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAction_hook_hook_toggled';

function QClipboard_hook_create(handle: QObjectH): QClipboard_hookH; cdecl; external QtShareName name QtNamePrefix + 'QClipboard_hook_create';
procedure QClipboard_hook_destroy(handle: QClipboard_hookH); cdecl; external QtShareName name QtNamePrefix + 'QClipboard_hook_destroy'; 
procedure QClipboard_hook_hook_selectionChanged(handle: QClipboard_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QClipboard_hook_hook_selectionChanged';
procedure QClipboard_hook_hook_dataChanged(handle: QClipboard_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QClipboard_hook_hook_dataChanged';

function QDesktopWidget_hook_create(handle: QObjectH): QDesktopWidget_hookH; cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_hook_create';
procedure QDesktopWidget_hook_destroy(handle: QDesktopWidget_hookH); cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_hook_destroy'; 
procedure QDesktopWidget_hook_hook_resized(handle: QDesktopWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_hook_hook_resized';
procedure QDesktopWidget_hook_hook_workAreaResized(handle: QDesktopWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QDesktopWidget_hook_hook_workAreaResized';

function QFrame_hook_create(handle: QObjectH): QFrame_hookH; cdecl; external QtShareName name QtNamePrefix + 'QFrame_hook_create';
procedure QFrame_hook_destroy(handle: QFrame_hookH); cdecl; external QtShareName name QtNamePrefix + 'QFrame_hook_destroy'; 

function QAbstractScrollArea_hook_create(handle: QObjectH): QAbstractScrollArea_hookH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_hook_create';
procedure QAbstractScrollArea_hook_destroy(handle: QAbstractScrollArea_hookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractScrollArea_hook_destroy'; 

function QAbstractSlider_hook_create(handle: QObjectH): QAbstractSlider_hookH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_hook_create';
procedure QAbstractSlider_hook_destroy(handle: QAbstractSlider_hookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_hook_destroy'; 
procedure QAbstractSlider_hook_hook_valueChanged(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_hook_hook_valueChanged';
procedure QAbstractSlider_hook_hook_sliderPressed(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_hook_hook_sliderPressed';
procedure QAbstractSlider_hook_hook_sliderMoved(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_hook_hook_sliderMoved';
procedure QAbstractSlider_hook_hook_sliderReleased(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_hook_hook_sliderReleased';
procedure QAbstractSlider_hook_hook_rangeChanged(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_hook_hook_rangeChanged';
procedure QAbstractSlider_hook_hook_actionTriggered(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSlider_hook_hook_actionTriggered';

function QScrollBar_hook_create(handle: QObjectH): QScrollBar_hookH; cdecl; external QtShareName name QtNamePrefix + 'QScrollBar_hook_create';
procedure QScrollBar_hook_destroy(handle: QScrollBar_hookH); cdecl; external QtShareName name QtNamePrefix + 'QScrollBar_hook_destroy'; 

function QMenu_hook_create(handle: QObjectH): QMenu_hookH; cdecl; external QtShareName name QtNamePrefix + 'QMenu_hook_create';
procedure QMenu_hook_destroy(handle: QMenu_hookH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_hook_destroy'; 
procedure QMenu_hook_hook_aboutToShow(handle: QMenu_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_hook_hook_aboutToShow';
procedure QMenu_hook_hook_triggered(handle: QMenu_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_hook_hook_triggered';
procedure QMenu_hook_hook_hovered(handle: QMenu_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QMenu_hook_hook_hovered';

function QMenuBar_hook_create(handle: QObjectH): QMenuBar_hookH; cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_hook_create';
procedure QMenuBar_hook_destroy(handle: QMenuBar_hookH); cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_hook_destroy'; 
procedure QMenuBar_hook_hook_triggered(handle: QMenuBar_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_hook_hook_triggered';
procedure QMenuBar_hook_hook_hovered(handle: QMenuBar_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QMenuBar_hook_hook_hovered';

function QButtonGroup_hook_create(handle: QObjectH): QButtonGroup_hookH; cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_hook_create';
procedure QButtonGroup_hook_destroy(handle: QButtonGroup_hookH); cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_hook_destroy'; 
procedure QButtonGroup_hook_hook_buttonClicked(handle: QButtonGroup_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_hook_hook_buttonClicked';
procedure QButtonGroup_hook_hook_buttonClicked2(handle: QButtonGroup_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QButtonGroup_hook_hook_buttonClicked2';

function QAbstractButton_hook_create(handle: QObjectH): QAbstractButton_hookH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_hook_create';
procedure QAbstractButton_hook_destroy(handle: QAbstractButton_hookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_hook_destroy'; 
procedure QAbstractButton_hook_hook_pressed(handle: QAbstractButton_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_hook_hook_pressed';
procedure QAbstractButton_hook_hook_released(handle: QAbstractButton_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_hook_hook_released';
procedure QAbstractButton_hook_hook_clicked(handle: QAbstractButton_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_hook_hook_clicked';
procedure QAbstractButton_hook_hook_clicked2(handle: QAbstractButton_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_hook_hook_clicked2';
procedure QAbstractButton_hook_hook_toggled(handle: QAbstractButton_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractButton_hook_hook_toggled';

function QPushButton_hook_create(handle: QObjectH): QPushButton_hookH; cdecl; external QtShareName name QtNamePrefix + 'QPushButton_hook_create';
procedure QPushButton_hook_destroy(handle: QPushButton_hookH); cdecl; external QtShareName name QtNamePrefix + 'QPushButton_hook_destroy'; 

function QLineEdit_hook_create(handle: QObjectH): QLineEdit_hookH; cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_hook_create';
procedure QLineEdit_hook_destroy(handle: QLineEdit_hookH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_hook_destroy'; 
procedure QLineEdit_hook_hook_textChanged(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_hook_hook_textChanged';
procedure QLineEdit_hook_hook_textEdited(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_hook_hook_textEdited';
procedure QLineEdit_hook_hook_cursorPositionChanged(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_hook_hook_cursorPositionChanged';
procedure QLineEdit_hook_hook_returnPressed(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_hook_hook_returnPressed';
procedure QLineEdit_hook_hook_editingFinished(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_hook_hook_editingFinished';
procedure QLineEdit_hook_hook_selectionChanged(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QLineEdit_hook_hook_selectionChanged';

function QTextEdit_hook_create(handle: QObjectH): QTextEdit_hookH; cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_hook_create';
procedure QTextEdit_hook_destroy(handle: QTextEdit_hookH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_hook_destroy'; 
procedure QTextEdit_hook_hook_textChanged(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_hook_hook_textChanged';
procedure QTextEdit_hook_hook_undoAvailable(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_hook_hook_undoAvailable';
procedure QTextEdit_hook_hook_redoAvailable(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_hook_hook_redoAvailable';
procedure QTextEdit_hook_hook_currentCharFormatChanged(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_hook_hook_currentCharFormatChanged';
procedure QTextEdit_hook_hook_copyAvailable(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_hook_hook_copyAvailable';
procedure QTextEdit_hook_hook_selectionChanged(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_hook_hook_selectionChanged';
procedure QTextEdit_hook_hook_cursorPositionChanged(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextEdit_hook_hook_cursorPositionChanged';

function QMainWindow_hook_create(handle: QObjectH): QMainWindow_hookH; cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_hook_create';
procedure QMainWindow_hook_destroy(handle: QMainWindow_hookH); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_hook_destroy'; 
procedure QMainWindow_hook_hook_iconSizeChanged(handle: QMainWindow_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_hook_hook_iconSizeChanged';
procedure QMainWindow_hook_hook_toolButtonStyleChanged(handle: QMainWindow_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QMainWindow_hook_hook_toolButtonStyleChanged';

function QToolBar_hook_create(handle: QObjectH): QToolBar_hookH; cdecl; external QtShareName name QtNamePrefix + 'QToolBar_hook_create';
procedure QToolBar_hook_destroy(handle: QToolBar_hookH); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_hook_destroy'; 
procedure QToolBar_hook_hook_actionTriggered(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_hook_hook_actionTriggered';
procedure QToolBar_hook_hook_movableChanged(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_hook_hook_movableChanged';
procedure QToolBar_hook_hook_allowedAreasChanged(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_hook_hook_allowedAreasChanged';
procedure QToolBar_hook_hook_orientationChanged(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_hook_hook_orientationChanged';
procedure QToolBar_hook_hook_iconSizeChanged(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_hook_hook_iconSizeChanged';
procedure QToolBar_hook_hook_toolButtonStyleChanged(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QToolBar_hook_hook_toolButtonStyleChanged';

function QLCDNumber_hook_create(handle: QObjectH): QLCDNumber_hookH; cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_hook_create';
procedure QLCDNumber_hook_destroy(handle: QLCDNumber_hookH); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_hook_destroy'; 
procedure QLCDNumber_hook_hook_overflow(handle: QLCDNumber_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QLCDNumber_hook_hook_overflow';

function QAbstractSpinBox_hook_create(handle: QObjectH): QAbstractSpinBox_hookH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_hook_create';
procedure QAbstractSpinBox_hook_destroy(handle: QAbstractSpinBox_hookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_hook_destroy'; 
procedure QAbstractSpinBox_hook_hook_editingFinished(handle: QAbstractSpinBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractSpinBox_hook_hook_editingFinished';

function QSpinBox_hook_create(handle: QObjectH): QSpinBox_hookH; cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_hook_create';
procedure QSpinBox_hook_destroy(handle: QSpinBox_hookH); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_hook_destroy'; 
procedure QSpinBox_hook_hook_valueChanged(handle: QSpinBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_hook_hook_valueChanged';
procedure QSpinBox_hook_hook_valueChanged2(handle: QSpinBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QSpinBox_hook_hook_valueChanged2';

function QDoubleSpinBox_hook_create(handle: QObjectH): QDoubleSpinBox_hookH; cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_hook_create';
procedure QDoubleSpinBox_hook_destroy(handle: QDoubleSpinBox_hookH); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_hook_destroy'; 
procedure QDoubleSpinBox_hook_hook_valueChanged(handle: QDoubleSpinBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_hook_hook_valueChanged';
procedure QDoubleSpinBox_hook_hook_valueChanged2(handle: QDoubleSpinBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QDoubleSpinBox_hook_hook_valueChanged2';

function QSplitter_hook_create(handle: QObjectH): QSplitter_hookH; cdecl; external QtShareName name QtNamePrefix + 'QSplitter_hook_create';
procedure QSplitter_hook_destroy(handle: QSplitter_hookH); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_hook_destroy'; 
procedure QSplitter_hook_hook_splitterMoved(handle: QSplitter_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QSplitter_hook_hook_splitterMoved';

function QSplitterHandle_hook_create(handle: QObjectH): QSplitterHandle_hookH; cdecl; external QtShareName name QtNamePrefix + 'QSplitterHandle_hook_create';
procedure QSplitterHandle_hook_destroy(handle: QSplitterHandle_hookH); cdecl; external QtShareName name QtNamePrefix + 'QSplitterHandle_hook_destroy'; 

function QWorkspace_hook_create(handle: QObjectH): QWorkspace_hookH; cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_hook_create';
procedure QWorkspace_hook_destroy(handle: QWorkspace_hookH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_hook_destroy'; 
procedure QWorkspace_hook_hook_windowActivated(handle: QWorkspace_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QWorkspace_hook_hook_windowActivated';

function QComboBox_hook_create(handle: QObjectH): QComboBox_hookH; cdecl; external QtShareName name QtNamePrefix + 'QComboBox_hook_create';
procedure QComboBox_hook_destroy(handle: QComboBox_hookH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_hook_destroy'; 
procedure QComboBox_hook_hook_editTextChanged(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_hook_hook_editTextChanged';
procedure QComboBox_hook_hook_activated(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_hook_hook_activated';
procedure QComboBox_hook_hook_activated2(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_hook_hook_activated2';
procedure QComboBox_hook_hook_highlighted(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_hook_hook_highlighted';
procedure QComboBox_hook_hook_highlighted2(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_hook_hook_highlighted2';
procedure QComboBox_hook_hook_currentIndexChanged(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_hook_hook_currentIndexChanged';
procedure QComboBox_hook_hook_currentIndexChanged2(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QComboBox_hook_hook_currentIndexChanged2';

function QCheckBox_hook_create(handle: QObjectH): QCheckBox_hookH; cdecl; external QtShareName name QtNamePrefix + 'QCheckBox_hook_create';
procedure QCheckBox_hook_destroy(handle: QCheckBox_hookH); cdecl; external QtShareName name QtNamePrefix + 'QCheckBox_hook_destroy'; 
procedure QCheckBox_hook_hook_stateChanged(handle: QCheckBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QCheckBox_hook_hook_stateChanged';

function QSlider_hook_create(handle: QObjectH): QSlider_hookH; cdecl; external QtShareName name QtNamePrefix + 'QSlider_hook_create';
procedure QSlider_hook_destroy(handle: QSlider_hookH); cdecl; external QtShareName name QtNamePrefix + 'QSlider_hook_destroy'; 

function QTextBrowser_hook_create(handle: QObjectH): QTextBrowser_hookH; cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_hook_create';
procedure QTextBrowser_hook_destroy(handle: QTextBrowser_hookH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_hook_destroy'; 
procedure QTextBrowser_hook_hook_backwardAvailable(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_hook_hook_backwardAvailable';
procedure QTextBrowser_hook_hook_forwardAvailable(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_hook_hook_forwardAvailable';
procedure QTextBrowser_hook_hook_sourceChanged(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_hook_hook_sourceChanged';
procedure QTextBrowser_hook_hook_highlighted(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_hook_hook_highlighted';
procedure QTextBrowser_hook_hook_highlighted2(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_hook_hook_highlighted2';
procedure QTextBrowser_hook_hook_anchorClicked(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTextBrowser_hook_hook_anchorClicked';

function QLabel_hook_create(handle: QObjectH): QLabel_hookH; cdecl; external QtShareName name QtNamePrefix + 'QLabel_hook_create';
procedure QLabel_hook_destroy(handle: QLabel_hookH); cdecl; external QtShareName name QtNamePrefix + 'QLabel_hook_destroy'; 

function QGroupBox_hook_create(handle: QObjectH): QGroupBox_hookH; cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_hook_create';
procedure QGroupBox_hook_destroy(handle: QGroupBox_hookH); cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_hook_destroy'; 
procedure QGroupBox_hook_hook_toggled(handle: QGroupBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QGroupBox_hook_hook_toggled';

function QTabWidget_hook_create(handle: QObjectH): QTabWidget_hookH; cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_hook_create';
procedure QTabWidget_hook_destroy(handle: QTabWidget_hookH); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_hook_destroy'; 
procedure QTabWidget_hook_hook_currentChanged(handle: QTabWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTabWidget_hook_hook_currentChanged';

function QTabBar_hook_create(handle: QObjectH): QTabBar_hookH; cdecl; external QtShareName name QtNamePrefix + 'QTabBar_hook_create';
procedure QTabBar_hook_destroy(handle: QTabBar_hookH); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_hook_destroy'; 
procedure QTabBar_hook_hook_currentChanged(handle: QTabBar_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTabBar_hook_hook_currentChanged';

function QProgressBar_hook_create(handle: QObjectH): QProgressBar_hookH; cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_hook_create';
procedure QProgressBar_hook_destroy(handle: QProgressBar_hookH); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_hook_destroy'; 
procedure QProgressBar_hook_hook_valueChanged(handle: QProgressBar_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QProgressBar_hook_hook_valueChanged';

function QStatusBar_hook_create(handle: QObjectH): QStatusBar_hookH; cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_hook_create';
procedure QStatusBar_hook_destroy(handle: QStatusBar_hookH); cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_hook_destroy'; 
procedure QStatusBar_hook_hook_messageChanged(handle: QStatusBar_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QStatusBar_hook_hook_messageChanged';

function QToolBox_hook_create(handle: QObjectH): QToolBox_hookH; cdecl; external QtShareName name QtNamePrefix + 'QToolBox_hook_create';
procedure QToolBox_hook_destroy(handle: QToolBox_hookH); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_hook_destroy'; 
procedure QToolBox_hook_hook_currentChanged(handle: QToolBox_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QToolBox_hook_hook_currentChanged';

function QToolButton_hook_create(handle: QObjectH): QToolButton_hookH; cdecl; external QtShareName name QtNamePrefix + 'QToolButton_hook_create';
procedure QToolButton_hook_destroy(handle: QToolButton_hookH); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_hook_destroy'; 
procedure QToolButton_hook_hook_triggered(handle: QToolButton_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QToolButton_hook_hook_triggered';

function QAbstractItemView_hook_create(handle: QObjectH): QAbstractItemView_hookH; cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_hook_create';
procedure QAbstractItemView_hook_destroy(handle: QAbstractItemView_hookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_hook_destroy'; 
procedure QAbstractItemView_hook_hook_pressed(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_hook_hook_pressed';
procedure QAbstractItemView_hook_hook_clicked(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_hook_hook_clicked';
procedure QAbstractItemView_hook_hook_doubleClicked(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_hook_hook_doubleClicked';
procedure QAbstractItemView_hook_hook_activated(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_hook_hook_activated';
procedure QAbstractItemView_hook_hook_entered(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_hook_hook_entered';
procedure QAbstractItemView_hook_hook_viewportEntered(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QAbstractItemView_hook_hook_viewportEntered';

function QListView_hook_create(handle: QObjectH): QListView_hookH; cdecl; external QtShareName name QtNamePrefix + 'QListView_hook_create';
procedure QListView_hook_destroy(handle: QListView_hookH); cdecl; external QtShareName name QtNamePrefix + 'QListView_hook_destroy'; 

function QListWidgetItem_hook_create(handle: QObjectH): QListWidgetItem_hookH; cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_hook_create';
procedure QListWidgetItem_hook_destroy(handle: QListWidgetItem_hookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidgetItem_hook_destroy'; 

function QListWidget_hook_create(handle: QObjectH): QListWidget_hookH; cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_create';
procedure QListWidget_hook_destroy(handle: QListWidget_hookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_destroy'; 
procedure QListWidget_hook_hook_itemPressed(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_hook_itemPressed';
procedure QListWidget_hook_hook_itemClicked(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_hook_itemClicked';
procedure QListWidget_hook_hook_itemDoubleClicked(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_hook_itemDoubleClicked';
procedure QListWidget_hook_hook_itemActivated(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_hook_itemActivated';
procedure QListWidget_hook_hook_itemEntered(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_hook_itemEntered';
procedure QListWidget_hook_hook_itemChanged(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_hook_itemChanged';
procedure QListWidget_hook_hook_currentItemChanged(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_hook_currentItemChanged';
procedure QListWidget_hook_hook_currentTextChanged(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_hook_currentTextChanged';
procedure QListWidget_hook_hook_currentRowChanged(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_hook_currentRowChanged';
procedure QListWidget_hook_hook_itemSelectionChanged(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QListWidget_hook_hook_itemSelectionChanged';

function QTreeView_hook_create(handle: QObjectH): QTreeView_hookH; cdecl; external QtShareName name QtNamePrefix + 'QTreeView_hook_create';
procedure QTreeView_hook_destroy(handle: QTreeView_hookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_hook_destroy'; 
procedure QTreeView_hook_hook_expanded(handle: QTreeView_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_hook_hook_expanded';
procedure QTreeView_hook_hook_collapsed(handle: QTreeView_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeView_hook_hook_collapsed';

function QTreeWidgetItem_hook_create(handle: QObjectH): QTreeWidgetItem_hookH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_hook_create';
procedure QTreeWidgetItem_hook_destroy(handle: QTreeWidgetItem_hookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidgetItem_hook_destroy'; 

function QTreeWidget_hook_create(handle: QObjectH): QTreeWidget_hookH; cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_create';
procedure QTreeWidget_hook_destroy(handle: QTreeWidget_hookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_destroy'; 
procedure QTreeWidget_hook_hook_itemPressed(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_hook_itemPressed';
procedure QTreeWidget_hook_hook_itemClicked(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_hook_itemClicked';
procedure QTreeWidget_hook_hook_itemDoubleClicked(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_hook_itemDoubleClicked';
procedure QTreeWidget_hook_hook_itemActivated(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_hook_itemActivated';
procedure QTreeWidget_hook_hook_itemEntered(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_hook_itemEntered';
procedure QTreeWidget_hook_hook_itemChanged(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_hook_itemChanged';
procedure QTreeWidget_hook_hook_itemExpanded(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_hook_itemExpanded';
procedure QTreeWidget_hook_hook_itemCollapsed(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_hook_itemCollapsed';
procedure QTreeWidget_hook_hook_currentItemChanged(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_hook_currentItemChanged';
procedure QTreeWidget_hook_hook_itemSelectionChanged(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QTreeWidget_hook_hook_itemSelectionChanged';

function QDialog_hook_create(handle: QObjectH): QDialog_hookH; cdecl; external QtShareName name QtNamePrefix + 'QDialog_hook_create';
procedure QDialog_hook_destroy(handle: QDialog_hookH); cdecl; external QtShareName name QtNamePrefix + 'QDialog_hook_destroy'; 
procedure QDialog_hook_hook_finished(handle: QDialog_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QDialog_hook_hook_finished';
procedure QDialog_hook_hook_accepted(handle: QDialog_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QDialog_hook_hook_accepted';
procedure QDialog_hook_hook_rejected(handle: QDialog_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QDialog_hook_hook_rejected';

function QProgressDialog_hook_create(handle: QObjectH): QProgressDialog_hookH; cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_hook_create';
procedure QProgressDialog_hook_destroy(handle: QProgressDialog_hookH); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_hook_destroy'; 
procedure QProgressDialog_hook_hook_canceled(handle: QProgressDialog_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QProgressDialog_hook_hook_canceled';

function QIODevice_hook_create(handle: QObjectH): QIODevice_hookH; cdecl; external QtShareName name QtNamePrefix + 'QIODevice_hook_create';
procedure QIODevice_hook_destroy(handle: QIODevice_hookH); cdecl; external QtShareName name QtNamePrefix + 'QIODevice_hook_destroy'; 
procedure QIODevice_hook_hook_readyRead(handle: QIODevice_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QIODevice_hook_hook_readyRead';
procedure QIODevice_hook_hook_bytesWritten(handle: QIODevice_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QIODevice_hook_hook_bytesWritten';
procedure QIODevice_hook_hook_aboutToClose(handle: QIODevice_hookH; hook: QHookH); cdecl; external QtShareName name QtNamePrefix + 'QIODevice_hook_hook_aboutToClose';



procedure initPWideStrings(CUPS, UOPS, LOPS, IPS, FPS: Pointer); cdecl; external QtShareName name QtNamePrefix + 'initPWideStrings';
procedure InitializePIntArray(GPP, GPL, SPL: Pointer); cdecl; external QtShareName name QtNamePrefix + 'initializePIntArray';
                       

implementation
uses SysUtils,Math;


// AnsiString Helpers
procedure CopyCharsToPAnsiString(Chars: PAnsiChar; var S: AnsiString); cdecl; export;
begin
  S := Chars;
end;

function CharsOfPAnsiString(var S: AnsiString): PAnsiChar; cdecl; export;
begin
  Result := PAnsiChar(Pointer(S));
end;

procedure InitPAnsiString(var S: PAnsiString); cdecl; export;
begin
  New(S);
end;

procedure FinalPAnsiString(var S: PAnsiString); cdecl; export;
begin
  Dispose(S);
end;

// WideString Helpers
procedure CopyUnicodeToPWideString(Unicode: PWideChar; var S: WideString;
  Len: Integer); cdecl; export;
{$IFDEF LINUX}
begin
  SetLength(S, Len);
  Move(Unicode[0],S[1],Len*2);
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
begin
  SetString(S, Unicode, Len);
end;
{$ENDIF}
{$IFDEF DARWIN}
begin
  SetString(S, Unicode, Len);
end;
{$ENDIF}


function UnicodeOfPWideString(var S: WideString): PWideChar; cdecl; export;
const
  cEmptyStr = '';
begin
  if @S = nil then
    Result := cEmptyStr
  else
    Result := PWideChar(Pointer(S));
end;

function LengthOfPWideString(var S: WideString): Integer; cdecl; export;
begin
  if @S <> nil then
    Result := Length(S)
  else
    Result := 0;
end;

procedure InitPWideString(var S: PWideString); cdecl; export;
begin
  New(S);
end;

procedure FinalPWideString(var S: PWideString); cdecl; export;
begin
  Dispose(S);
end;


// Int array helpers
function GetIntsPtr(PA : PIntArray): PInteger; cdecl; export;
begin
  Result := @PA^[0];
end;

function GetIntsLength(PA: PIntArray): Integer; cdecl; export;
begin
  Result := Length(PA^);
end;

procedure SetIntsLength(var PA: TIntArray; Len: Integer); cdecl; export;
begin
  SetLength(PA, Len);
end;

initialization

initPWideStrings(@CopyUnicodeToPWideString, 
                 @UnicodeOfPWideString,
                 @LengthOfPWideString, 
                 @InitPWideString, 
                 @FinalPWideString);
                    
InitializePIntArray(@GetIntsPtr, 
                    @GetIntsLength, 
                    @SetIntsLength);

SetExceptionMask([exDenormalized,exInvalidOp,exOverflow,exPrecision,exUnderflow,exZeroDivide]);
end.


