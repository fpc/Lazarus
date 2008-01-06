unit qt4;

{ Version : 1.57 }

{$ifdef fpc}
  {$mode delphi}
{$endif}

interface

uses Types;

{$MINENUMSIZE 4}


const
  QT_VERSION = 4 shl 16 + 2 shl 8 + 3;
  
{$IFDEF MSWINDOWS}
  QtIntf = 'libqt4intf.dll';
{$ENDIF}
{$IFDEF UNIX}
  QtIntf = 'libqt4intf.so';
{$ENDIF}
{$IFDEF DARWIN}
  QtIntf = '';
{$ENDIF}


type

{$ifndef fpc}
  PPtrInt = ^PtrInt;
  PtrInt = longint; // 32bit dcc 
  qword = type int64;
{$endif}

  PLong = ^Long;
{$ifdef CPU64 and not WIN64}
   Long = Int64;
{$else}
   Long = LongInt;
{$endif}
  
  

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
  
  TCoreApplicationEventFilter = function(Msg:PChar;Res:PLong):boolean cdecl;
  TAbstractEventFilter = function(Msg:PChar):boolean cdecl;
    

  PIntArray = ^TIntArray;
  TIntArray = array of PtrInt;
  
  
  
const
  NullHook: QHookH = (Code: nil; Data: nil);

type

{$IFDEF DARWIN}
  TQtPoint = packed record 
    y : LongInt;
    x : LongInt;
  end;
{$ELSE}
  TQtPoint = packed record 
    x : LongInt;
    y : LongInt;
  end;
{$ENDIF}  
  PQtPoint = ^TQtPoint;
  
{$IFDEF DARWIN}
    EventHandlerRef                     = ^LongInt;
    EventRef                            = ^LongInt;
    RgnHandle                           = ^LongInt;
    MenuHandle                          = ^LongInt;
    MenuRef                             = MenuHandle;
    EventHandlerCallRef                 = ^LongInt;
    CGImageRef                          = Pointer;    
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
  QLCLMessageEventH = class(QEventH) end;
  QActionEventH = class(QEventH) end;
  QChildEventH = class(QEventH) end;
  QCloseEventH = class(QEventH) end;
  QDragLeaveEventH = class(QEventH) end;
  QDropEventH = class(QEventH) end;
    QDragMoveEventH = class(QDropEventH) end;
      QDragEnterEventH = class(QDragMoveEventH) end;
  QDynamicPropertyChangeEventH = class(QEventH) end;
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
QGraphicsItemH = class(TObject) end;
  QAbstractGraphicsShapeItemH = class(QGraphicsItemH) end;
    QGraphicsEllipseItemH = class(QAbstractGraphicsShapeItemH) end;
    QGraphicsPathItemH = class(QAbstractGraphicsShapeItemH) end;
    QGraphicsPolygonItemH = class(QAbstractGraphicsShapeItemH) end;
    QGraphicsRectItemH = class(QAbstractGraphicsShapeItemH) end;
  QGraphicsItemGroupH = class(QGraphicsItemH) end;
  QGraphicsLineItemH = class(QGraphicsItemH) end;
  QGraphicsPixmapItemH = class(QGraphicsItemH) end;
  QGraphicsTextItemH = class(QGraphicsItemH) end;
QIconH = class(TObject) end;
QIconEngineH = class(TObject) end;
QImageIOHandlerH = class(TObject) end;
QImageReaderH = class(TObject) end;
QImageWriterH = class(TObject) end;
QItemEditorCreatorBaseH = class(TObject) end;
QItemEditorFactoryH = class(TObject) end;
QItemSelectionRangeH = class(TObject) end;
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
QMetaClassInfoH = class(TObject) end;
QMetaEnumH = class(TObject) end;
QMetaMethodH = class(TObject) end;
QMetaObjectH = class(TObject) end;
QMetaPropertyH = class(TObject) end;
QMimeSourceH = class(TObject) end;
QModelIndexH = class(TObject) end;
QObjectH = class(TObject) end;
  QAbstractEventDispatcherH = class(QObjectH) end;
  QAbstractItemDelegateH = class(QObjectH) end;
    QItemDelegateH = class(QAbstractItemDelegateH) end;
      QLCLItemDelegateH = class(QItemDelegateH) end;
  QAbstractItemModelH = class(QObjectH) end;
    QAbstractListModelH = class(QAbstractItemModelH) end;
    QAbstractTableModelH = class(QAbstractItemModelH) end;
    QStandardItemModelH = class(QAbstractItemModelH) end;
  QAbstractTextDocumentLayoutH = class(QObjectH) end;
  QActionH = class(QObjectH) end;
  QActionGroupH = class(QObjectH) end;
  QButtonGroupH = class(QObjectH) end;
  QClipboardH = class(QObjectH) end;
  QCompleterH = class(QObjectH) end;
  QCoreApplicationH = class(QObjectH) end;
    QApplicationH = class(QCoreApplicationH) end;
  QDragH = class(QObjectH) end;
  QEventLoopH = class(QObjectH) end;
  QGraphicsSceneH = class(QObjectH) end;
  QIODeviceH = class(QObjectH) end;
  QInputContextH = class(QObjectH) end;
  QItemSelectionModelH = class(QObjectH) end;
  QMimeDataH = class(QObjectH) end;
  QMovieH = class(QObjectH) end;
  QSessionManagerH = class(QObjectH) end;
  QSocketNotifierH = class(QObjectH) end;
  QStyleH = class(QObjectH) end;
  QSystemTrayIconH = class(QObjectH) end;
  QTextDocumentH = class(QObjectH) end;
  QTextObjectH = class(QObjectH) end;
    QTextBlockGroupH = class(QTextObjectH) end;
      QTextListH = class(QTextBlockGroupH) end;
    QTextFrameH = class(QTextObjectH) end;
      QTextTableH = class(QTextFrameH) end;
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
      QLCLAbstractSpinBoxH = class(QAbstractSpinBoxH) end;
      QDoubleSpinBoxH = class(QAbstractSpinBoxH) end;
      QSpinBoxH = class(QAbstractSpinBoxH) end;
    QCalendarWidgetH = class(QWidgetH) end;
    QComboBoxH = class(QWidgetH) end;
    QDesktopWidgetH = class(QWidgetH) end;
    QDialogH = class(QWidgetH) end;
      QAbstractPrintDialogH = class(QDialogH) end;
        QPrintDialogH = class(QAbstractPrintDialogH) end;
      QColorDialogH = class(QDialogH) end;
      QFileDialogH = class(QDialogH) end;
      QFontDialogH = class(QDialogH) end;
      QInputDialogH = class(QDialogH) end;
      QMessageBoxH = class(QDialogH) end;
      QProgressDialogH = class(QDialogH) end;
    QDockWidgetH = class(QWidgetH) end;
    QFrameH = class(QWidgetH) end;
      QAbstractScrollAreaH = class(QFrameH) end;
        QLCLAbstractScrollAreaH = class(QAbstractScrollAreaH) end;
        QAbstractItemViewH = class(QAbstractScrollAreaH) end;
          QHeaderViewH = class(QAbstractItemViewH) end;
          QListViewH = class(QAbstractItemViewH) end;
            QListWidgetH = class(QListViewH) end;
          QTableViewH = class(QAbstractItemViewH) end;
            QTableWidgetH = class(QTableViewH) end;
          QTreeViewH = class(QAbstractItemViewH) end;
            QTreeWidgetH = class(QTreeViewH) end;
        QGraphicsViewH = class(QAbstractScrollAreaH) end;
        QScrollAreaH = class(QAbstractScrollAreaH) end;
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
    QRubberBandH = class(QWidgetH) end;
    QSizeGripH = class(QWidgetH) end;
    QSplitterHandleH = class(QWidgetH) end;
    QStatusBarH = class(QWidgetH) end;
    QTabBarH = class(QWidgetH) end;
    QTabWidgetH = class(QWidgetH) end;
      QLCLTabWidgetH = class(QTabWidgetH) end;
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
QPainterPathStrokerH = class(TObject) end;
QPaletteH = class(TObject) end;
QPenH = class(TObject) end;
QPersistentModelIndexH = class(TObject) end;
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
QStandardItemH = class(TObject) end;
QStringH = class(TObject) end;
QStyleFactoryH = class(TObject) end;
QStyleHintReturnH = class(TObject) end;
  QStyleHintReturnMaskH = class(QStyleHintReturnH) end;
QStyleOptionH = class(TObject) end;
  QStyleOptionButtonH = class(QStyleOptionH) end;
  QStyleOptionComplexH = class(QStyleOptionH) end;
    QStyleOptionComboBoxH = class(QStyleOptionComplexH) end;
    QStyleOptionGroupBoxH = class(QStyleOptionComplexH) end;
    QStyleOptionQ3ListViewH = class(QStyleOptionComplexH) end;
    QStyleOptionSizeGripH = class(QStyleOptionComplexH) end;
    QStyleOptionSliderH = class(QStyleOptionComplexH) end;
    QStyleOptionSpinBoxH = class(QStyleOptionComplexH) end;
    QStyleOptionTitleBarH = class(QStyleOptionComplexH) end;
    QStyleOptionToolButtonH = class(QStyleOptionComplexH) end;
  QStyleOptionDockWidgetH = class(QStyleOptionH) end;
  QStyleOptionFocusRectH = class(QStyleOptionH) end;
  QStyleOptionFrameH = class(QStyleOptionH) end;
    QStyleOptionFrameV2H = class(QStyleOptionFrameH) end;
  QStyleOptionGraphicsItemH = class(QStyleOptionH) end;
  QStyleOptionHeaderH = class(QStyleOptionH) end;
  QStyleOptionMenuItemH = class(QStyleOptionH) end;
  QStyleOptionProgressBarH = class(QStyleOptionH) end;
    QStyleOptionProgressBarV2H = class(QStyleOptionProgressBarH) end;
  QStyleOptionQ3DockWindowH = class(QStyleOptionH) end;
  QStyleOptionQ3ListViewItemH = class(QStyleOptionH) end;
  QStyleOptionRubberBandH = class(QStyleOptionH) end;
  QStyleOptionTabH = class(QStyleOptionH) end;
    QStyleOptionTabV2H = class(QStyleOptionTabH) end;
  QStyleOptionTabBarBaseH = class(QStyleOptionH) end;
  QStyleOptionTabWidgetFrameH = class(QStyleOptionH) end;
  QStyleOptionToolBarH = class(QStyleOptionH) end;
  QStyleOptionToolBoxH = class(QStyleOptionH) end;
  QStyleOptionViewItemH = class(QStyleOptionH) end;
    QStyleOptionViewItemV2H = class(QStyleOptionViewItemH) end;
QSystemLocaleH = class(TObject) end;
QTableWidgetItemH = class(TObject) end;
QTableWidgetSelectionRangeH = class(TObject) end;
QTextBlockH = class(TObject) end;
QTextCursorH = class(TObject) end;
QTextDocumentFragmentH = class(TObject) end;
QTextFormatH = class(TObject) end;
  QTextBlockFormatH = class(QTextFormatH) end;
  QTextCharFormatH = class(QTextFormatH) end;
    QTextImageFormatH = class(QTextCharFormatH) end;
  QTextFrameFormatH = class(QTextFormatH) end;
    QTextTableFormatH = class(QTextFrameFormatH) end;
QTextOptionH = class(TObject) end;
QTimeH = class(TObject) end;
QToolTipH = class(TObject) end;
QTreeWidgetItemH = class(TObject) end;
QUrlH = class(TObject) end;
QVariantH = class(TObject) end;
QVectorH = class(TObject) end;
  QPolygonH = class(QVectorH) end;
  QPolygonFH = class(QVectorH) end;
QX11InfoH = class(TObject) end;

QPainterPathElementH = class(TObject) end;
QObject_hookH = class(TObject) end;
QEvent_hookH = class(QObject_hookH) end;
QTimerEvent_hookH = class(QEvent_hookH) end;
QChildEvent_hookH = class(QEvent_hookH) end;
QDynamicPropertyChangeEvent_hookH = class(QEvent_hookH) end;
QEventLoop_hookH = class(QObject_hookH) end;
QCoreApplication_hookH = class(QObject_hookH) end;
QTimer_hookH = class(QObject_hookH) end;
QModelIndex_hookH = class(QObject_hookH) end;
QPersistentModelIndex_hookH = class(QObject_hookH) end;
QAbstractItemModel_hookH = class(QObject_hookH) end;
QAbstractTableModel_hookH = class(QAbstractItemModel_hookH) end;
QAbstractListModel_hookH = class(QAbstractItemModel_hookH) end;
QApplication_hookH = class(QCoreApplication_hookH) end;
QWidget_hookH = class(QObject_hookH) end;
QAction_hookH = class(QObject_hookH) end;
QClipboard_hookH = class(QObject_hookH) end;
QDesktopWidget_hookH = class(QWidget_hookH) end;
QDrag_hookH = class(QObject_hookH) end;
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
QCalendarWidget_hookH = class(QWidget_hookH) end;
QAbstractItemView_hookH = class(QAbstractScrollArea_hookH) end;
QListView_hookH = class(QAbstractItemView_hookH) end;
QListWidgetItem_hookH = class(QObject_hookH) end;
QListWidget_hookH = class(QListView_hookH) end;
QTreeView_hookH = class(QAbstractItemView_hookH) end;
QTreeWidgetItem_hookH = class(QObject_hookH) end;
QTreeWidget_hookH = class(QTreeView_hookH) end;
QHeaderView_hookH = class(QAbstractItemView_hookH) end;
QStandardItem_hookH = class(QObject_hookH) end;
QStandardItemModel_hookH = class(QAbstractItemModel_hookH) end;
QAbstractItemDelegate_hookH = class(QObject_hookH) end;
QTableView_hookH = class(QAbstractItemView_hookH) end;
QTableWidgetSelectionRange_hookH = class(QObject_hookH) end;
QTableWidgetItem_hookH = class(QObject_hookH) end;
QTableWidget_hookH = class(QTableView_hookH) end;
QItemSelectionRange_hookH = class(QObject_hookH) end;
QItemSelectionModel_hookH = class(QObject_hookH) end;
QDialog_hookH = class(QWidget_hookH) end;
QFileDialog_hookH = class(QDialog_hookH) end;
QProgressDialog_hookH = class(QDialog_hookH) end;
QSystemTrayIcon_hookH = class(QObject_hookH) end;
QGraphicsScene_hookH = class(QObject_hookH) end;
QIODevice_hookH = class(QObject_hookH) end;

  TPictureIOHandler = procedure(Pic: QPictureIOH) cdecl;
  TEventFilterMethod = function (Sender: QObjectH; Event: QEventH): Boolean of object cdecl;

QLCLItemDelegate_sizeHint_Override = procedure (option: QStyleOptionViewItemH; index: QModelIndexH; Size: PSize) of object cdecl;
QLCLItemDelegate_paint_Override = procedure (painter : QPainterH; option: QStyleOptionViewItemH; index: QModelIndexH) of object cdecl;
QLCLItemDelegate_createEditor_Override = procedure (parent : QWidgetH; option: QStyleOptionViewItemH; index: QModelIndexH; out editor: QWidgetH) of object cdecl; 
QLCLItemDelegate_setEditorData_Override = procedure (editor : QWidgetH; index: QModelIndexH) of object cdecl;
QLCLItemDelegate_setModelData_Override = procedure (editor : QWidgetH; model: QAbstractItemModelH; index: QModelIndexH) of object cdecl;
QLCLItemDelegate_updateEditorGeometry_Override = procedure (editor : QWidgetH; option: QStyleOptionViewItemH; index: QModelIndexH) of object cdecl;
QLCLItemDelegate_editorEvent_Override = procedure (event : QEventH; model: QAbstractItemModelH; option: QStyleOptionViewItemH; index: QModelIndexH; retval: PBoolean) of object cdecl;
QLCLAbstractScrollArea_viewportEvent_Override = procedure (event: QEventH; retval: PBoolean) of object cdecl;
function QtPoint(X,Y:integer): TQtPoint;
function QObject_hook_create(handle : QObjectH) : QObject_hookH; cdecl; external QtIntf name 'QObject_hook_create';
procedure QObject_hook_destroy(handle : QObject_hookH ); cdecl; external QtIntf name 'QObject_hook_destroy';
procedure QObject_hook_hook_events(handle : QObject_hookH; hook : QHookH); cdecl; external QtIntf name 'QObject_hook_hook_events';
procedure QObject_hook_hook_destroyed(handle : QObject_hookH; hook : QHookH); cdecl; external QtIntf name 'QObject_hook_hook_destroyed';

type
  QtGlobalColor = ( // Qt::GlobalColor (1)
    Qtcolor0, Qtcolor1, Qtblack, Qtwhite, QtdarkGray, Qtgray, QtlightGray, Qtred, Qtgreen, Qtblue, Qtcyan, Qtmagenta, Qtyellow, QtdarkRed, QtdarkGreen, QtdarkBlue, QtdarkCyan, QtdarkMagenta, 
    QtdarkYellow, Qttransparent );

  QtSortOrder = ( // Qt::SortOrder (1)
    QtAscendingOrder, QtDescendingOrder );

  QtTextElideMode = ( // Qt::TextElideMode (1)
    QtElideLeft, QtElideRight, QtElideMiddle, QtElideNone );

  QtBGMode = ( // Qt::BGMode (1)
    QtTransparentMode, QtOpaqueMode );

  QtArrowType = ( // Qt::ArrowType (1)
    QtNoArrow, QtUpArrow, QtDownArrow, QtLeftArrow, QtRightArrow );

  QtPenStyle = ( // Qt::PenStyle (1)
    QtNoPen, QtSolidLine, QtDashLine, QtDotLine, QtDashDotLine, QtDashDotDotLine, QtCustomDashLine );

  QtUIEffect = ( // Qt::UIEffect (1)
    QtUI_General, QtUI_AnimateMenu, QtUI_FadeMenu, QtUI_AnimateCombo, QtUI_AnimateTooltip, QtUI_FadeTooltip, QtUI_AnimateToolBox );

  QtTextFormat = ( // Qt::TextFormat (1)
    QtPlainText, QtRichText, QtAutoText, QtLogText );

  QtAspectRatioMode = ( // Qt::AspectRatioMode (1)
    QtIgnoreAspectRatio, QtKeepAspectRatio, QtKeepAspectRatioByExpanding );

  QtAnchorAttribute = ( // Qt::AnchorAttribute (1)
    QtAnchorName, QtAnchorHref );

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
    QtNoContextMenu, QtDefaultContextMenu, QtActionsContextMenu, QtCustomContextMenu, QtPreventContextMenu );

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
  QtGroupSwitchModifier =   $40000000;
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
  QtApplicationAttribute = (  //Qt::ApplicationAttribute (2)
    QtAA_ImmediateWidgetCreation = 0,
    QtAA_AttributeCount );

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
  QtPenCapStyle = (  //Qt::PenCapStyle (2s)
    QtFlatCap = $00,
    QtSquareCap = $10,
    QtRoundCap = $20,
    QtMPenCapStyle = $30 );

  QtPenJoinStyle = (  //Qt::PenJoinStyle (2s)
    QtMiterJoin = $00,
    QtBevelJoin = $40,
    QtRoundJoin = $80,
    QtSvgMiterJoin = $100,
    QtMPenJoinStyle = $1c0 );

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

  QtDockWidgetAreaSizes = (  //Qt::DockWidgetAreaSizes (2s)
    QtNDockWidgetAreas = 4 );

  QtToolBarAreaSizes = (  //Qt::ToolBarAreaSizes (2s)
    QtNToolBarAreas = 4 );

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

  QtItemSelectionMode = (  //Qt::ItemSelectionMode (2s)
    QtContainsItemShape = $0,
    QtIntersectsItemShape = $1,
    QtContainsItemBoundingRect = $2,
    QtIntersectsItemBoundingRect = $3 );

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
    QtBackgroundRole = 8,
    QtForegroundRole = 9,
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
  QtMatchFixedString =   8;
  QtMatchCaseSensitive =   16;
  QtMatchWrap =   32;
  QtMatchRecursive =   64;


type
  QInternalRelayoutType = ( // QInternal::RelayoutType (1)
    QInternalRelayoutNormal, QInternalRelayoutDragging, QInternalRelayoutDropped );

  QInternalCallback = ( // QInternal::Callback (1)
    QInternalConnectCallback, QInternalDisconnectCallback, QInternalAdoptCurrentThread, QInternalLastCallback );

  QInternalInternalFunction = ( // QInternal::InternalFunction (1)
    QInternalCreateThreadForAdoption, QInternalRefAdoptedThread, QInternalDerefAdoptedThread, QInternalSetCurrentThreadToMainThread, QInternalLastInternalFunction );

  QInternalPaintDeviceFlags = (  //QInternal::PaintDeviceFlags (2s)
    QInternalUnknownDevice = $00,
    QInternalWidget = $01,
    QInternalPixmap = $02,
    QInternalImage = $03,
    QInternalPrinter = $04,
    QInternalPicture = $05,
    QInternalPbuffer = $06,
    QInternalFramebufferObject = $07,
    QInternalCustomRaster = $08 );

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
    QtCustomizeWindowHint = 33554432 { $2000000 };

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
    QtWA_AlwaysShowToolTips = 84 { $54 };
    QtWA_MacOpaqueSizeGrip = 85 { $55 };
    QtWA_SetStyle = 86 { $56 };
    QtWA_AttributeCount = 87 { $57 };

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
    QtKey_Cancel = 16908289 { $1020001 };
    QtKey_Printer = 16908290 { $1020002 };
    QtKey_Execute = 16908291 { $1020003 };
    QtKey_Sleep = 16908292 { $1020004 };
    QtKey_Play = 16908293 { $1020005 };
    QtKey_Zoom = 16908294 { $1020006 };
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
    QtOpenHandCursor = 17 { $11 };
    QtClosedHandCursor = 18 { $12 };
    QtLastCursor = 18 { $12 };
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
    QtNoDockWidgetArea = 0 { $0 };

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
    QtNoToolBarArea = 0 { $0 };

type
  QtDateFormat = cardinal; //  Qt::DateFormat (4)

const
    QtTextDate = 0 { $0 };
    QtISODate = 1 { $1 };
    QtSystemLocaleDate = 2 { $2 };
    QtLocalDate = 2 { $2 };
    QtLocaleDate = 3 { $3 };

type
  QtTextInteractionFlag = cardinal; //  Qt::TextInteractionFlag (4)
  QtTextInteractionFlags = QtTextInteractionFlag; // QFlags<>

const
    QtNoTextInteraction = 0 { $0 };
    QtTextSelectableByMouse = 1 { $1 };
    QtTextSelectableByKeyboard = 2 { $2 };
    QtLinksAccessibleByMouse = 4 { $4 };
    QtLinksAccessibleByKeyboard = 8 { $8 };
    QtTextEditable = 16 { $10 };
    QtTextEditorInteraction = 19 { $13 };
    QtTextBrowserInteraction = 13 { $d };



function QObject_create(parent: QObjectH = nil): QObjectH; cdecl; external QtIntf name 'QObject_create';
procedure QObject_destroy(handle: QObjectH); cdecl; external QtIntf name 'QObject_destroy'; 
function QObject_event(handle: QObjectH; p1: QEventH): Boolean; cdecl; external QtIntf name 'QObject_event';
function QObject_eventFilter(handle: QObjectH; p1: QObjectH; p2: QEventH): Boolean; cdecl; external QtIntf name 'QObject_eventFilter';
procedure QObject_tr(retval: PWideString; sourceText: PAnsiChar; comment: PAnsiChar = nil; n: Integer = -1); cdecl; external QtIntf name 'QObject_tr';
procedure QObject_trUtf8(retval: PWideString; sourceText: PAnsiChar; comment: PAnsiChar = nil; n: Integer = -1); cdecl; external QtIntf name 'QObject_trUtf8';
function QObject_metaObject(handle: QObjectH): QMetaObjectH; cdecl; external QtIntf name 'QObject_metaObject';
procedure QObject_objectName(handle: QObjectH; retval: PWideString); cdecl; external QtIntf name 'QObject_objectName';
procedure QObject_setObjectName(handle: QObjectH; name: PWideString); cdecl; external QtIntf name 'QObject_setObjectName';
function QObject_isWidgetType(handle: QObjectH): Boolean; cdecl; external QtIntf name 'QObject_isWidgetType';
function QObject_signalsBlocked(handle: QObjectH): Boolean; cdecl; external QtIntf name 'QObject_signalsBlocked';
function QObject_blockSignals(handle: QObjectH; b: Boolean): Boolean; cdecl; external QtIntf name 'QObject_blockSignals';
function QObject_thread(handle: QObjectH): QThreadH; cdecl; external QtIntf name 'QObject_thread';
procedure QObject_moveToThread(handle: QObjectH; thread: QThreadH); cdecl; external QtIntf name 'QObject_moveToThread';
function QObject_startTimer(handle: QObjectH; interval: Integer): Integer; cdecl; external QtIntf name 'QObject_startTimer';
procedure QObject_killTimer(handle: QObjectH; id: Integer); cdecl; external QtIntf name 'QObject_killTimer';
procedure QObject_setParent(handle: QObjectH; p1: QObjectH); cdecl; external QtIntf name 'QObject_setParent';
procedure QObject_installEventFilter(handle: QObjectH; p1: QObjectH); cdecl; external QtIntf name 'QObject_installEventFilter';
procedure QObject_removeEventFilter(handle: QObjectH; p1: QObjectH); cdecl; external QtIntf name 'QObject_removeEventFilter';
function QObject_connect(sender: QObjectH; signal: PAnsiChar; receiver: QObjectH; member: PAnsiChar; p5: QtConnectionType = QtAutoConnection): Boolean; overload; cdecl; external QtIntf name 'QObject_connect';
function QObject_connect(handle: QObjectH; sender: QObjectH; signal: PAnsiChar; member: PAnsiChar; _type: QtConnectionType = QtAutoConnection): Boolean; overload; cdecl; external QtIntf name 'QObject_connect2';
function QObject_disconnect(sender: QObjectH; signal: PAnsiChar; receiver: QObjectH; member: PAnsiChar): Boolean; overload; cdecl; external QtIntf name 'QObject_disconnect';
function QObject_disconnect(handle: QObjectH; receiver: QObjectH; member: PAnsiChar = nil): Boolean; overload; cdecl; external QtIntf name 'QObject_disconnect3';
procedure QObject_dumpObjectTree(handle: QObjectH); cdecl; external QtIntf name 'QObject_dumpObjectTree';
procedure QObject_dumpObjectInfo(handle: QObjectH); cdecl; external QtIntf name 'QObject_dumpObjectInfo';
function QObject_setProperty(handle: QObjectH; name: PAnsiChar; value: QVariantH): Boolean; cdecl; external QtIntf name 'QObject_setProperty';
procedure QObject_property(handle: QObjectH; retval: QVariantH; name: PAnsiChar); cdecl; external QtIntf name 'QObject_property';
function QObject_registerUserData(): LongWord; cdecl; external QtIntf name 'QObject_registerUserData';
function QObject_parent(handle: QObjectH): QObjectH; cdecl; external QtIntf name 'QObject_parent';
function QObject_inherits(handle: QObjectH; classname: PAnsiChar): Boolean; cdecl; external QtIntf name 'QObject_inherits';
procedure QObject_deleteLater(handle: QObjectH); cdecl; external QtIntf name 'QObject_deleteLater';


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
    QEventGraphicsSceneMouseMove = 155,
    QEventGraphicsSceneMousePress = 156,
    QEventGraphicsSceneMouseRelease = 157,
    QEventGraphicsSceneMouseDoubleClick = 158,
    QEventGraphicsSceneContextMenu = 159,
    QEventGraphicsSceneHoverEnter = 160,
    QEventGraphicsSceneHoverMove = 161,
    QEventGraphicsSceneHoverLeave = 162,
    QEventGraphicsSceneHelp = 163,
    QEventGraphicsSceneDragEnter = 164,
    QEventGraphicsSceneDragMove = 165,
    QEventGraphicsSceneDragLeave = 166,
    QEventGraphicsSceneDrop = 167,
    QEventGraphicsSceneWheel = 168,
    QEventKeyboardLayoutChange = 169,
    QEventDynamicPropertyChange = 170,
    QEventTabletEnterProximity = 171,
    QEventTabletLeaveProximity = 172,
    QEventUser = 1000,
    QEventMaxUser = 65535 );

function QEvent_create(_type: QEventType): QEventH; cdecl; external QtIntf name 'QEvent_create';
procedure QEvent_destroy(handle: QEventH); cdecl; external QtIntf name 'QEvent_destroy'; 
function QEvent_type(handle: QEventH): QEventType; cdecl; external QtIntf name 'QEvent_type';
function QEvent_spontaneous(handle: QEventH): Boolean; cdecl; external QtIntf name 'QEvent_spontaneous';
procedure QEvent_setAccepted(handle: QEventH; accepted: Boolean); cdecl; external QtIntf name 'QEvent_setAccepted';
function QEvent_isAccepted(handle: QEventH): Boolean; cdecl; external QtIntf name 'QEvent_isAccepted';
procedure QEvent_accept(handle: QEventH); cdecl; external QtIntf name 'QEvent_accept';
procedure QEvent_ignore(handle: QEventH); cdecl; external QtIntf name 'QEvent_ignore';

function QTimerEvent_create(timerId: Integer): QTimerEventH; cdecl; external QtIntf name 'QTimerEvent_create';
procedure QTimerEvent_destroy(handle: QTimerEventH); cdecl; external QtIntf name 'QTimerEvent_destroy'; 
function QTimerEvent_timerId(handle: QTimerEventH): Integer; cdecl; external QtIntf name 'QTimerEvent_timerId';

function QChildEvent_create(_type: QEventType; child: QObjectH): QChildEventH; cdecl; external QtIntf name 'QChildEvent_create';
procedure QChildEvent_destroy(handle: QChildEventH); cdecl; external QtIntf name 'QChildEvent_destroy'; 
function QChildEvent_child(handle: QChildEventH): QObjectH; cdecl; external QtIntf name 'QChildEvent_child';
function QChildEvent_added(handle: QChildEventH): Boolean; cdecl; external QtIntf name 'QChildEvent_added';
function QChildEvent_polished(handle: QChildEventH): Boolean; cdecl; external QtIntf name 'QChildEvent_polished';
function QChildEvent_removed(handle: QChildEventH): Boolean; cdecl; external QtIntf name 'QChildEvent_removed';

function QDynamicPropertyChangeEvent_create(name: QByteArrayH): QDynamicPropertyChangeEventH; cdecl; external QtIntf name 'QDynamicPropertyChangeEvent_create';
procedure QDynamicPropertyChangeEvent_destroy(handle: QDynamicPropertyChangeEventH); cdecl; external QtIntf name 'QDynamicPropertyChangeEvent_destroy'; 
procedure QDynamicPropertyChangeEvent_propertyName(handle: QDynamicPropertyChangeEventH; retval: QByteArrayH); cdecl; external QtIntf name 'QDynamicPropertyChangeEvent_propertyName';

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

function QEventLoop_create(parent: QObjectH = nil): QEventLoopH; cdecl; external QtIntf name 'QEventLoop_create';
procedure QEventLoop_destroy(handle: QEventLoopH); cdecl; external QtIntf name 'QEventLoop_destroy'; 
function QEventLoop_processEvents(handle: QEventLoopH; flags: QEventLoopProcessEventsFlags = QEventLoopAllEvents): Boolean; overload; cdecl; external QtIntf name 'QEventLoop_processEvents';
procedure QEventLoop_processEvents(handle: QEventLoopH; flags: QEventLoopProcessEventsFlags; maximumTime: Integer); overload; cdecl; external QtIntf name 'QEventLoop_processEvents2';
function QEventLoop_exec(handle: QEventLoopH; flags: QEventLoopProcessEventsFlags = QEventLoopAllEvents): Integer; cdecl; external QtIntf name 'QEventLoop_exec';
procedure QEventLoop_exit(handle: QEventLoopH; returnCode: Integer = 0); cdecl; external QtIntf name 'QEventLoop_exit';
function QEventLoop_isRunning(handle: QEventLoopH): Boolean; cdecl; external QtIntf name 'QEventLoop_isRunning';
procedure QEventLoop_wakeUp(handle: QEventLoopH); cdecl; external QtIntf name 'QEventLoop_wakeUp';
procedure QEventLoop_quit(handle: QEventLoopH); cdecl; external QtIntf name 'QEventLoop_quit';

type
  QCoreApplicationEncoding = cardinal; //  QCoreApplication::Encoding (4)

const
    QCoreApplicationCodecForTr = 0 { $0 };
    QCoreApplicationUnicodeUTF8 = 1 { $1 };
    QCoreApplicationDefaultCodec = 0 { $0 };


function QCoreApplication_create(argc: PInteger; argv: PPAnsiChar): QCoreApplicationH; cdecl; external QtIntf name 'QCoreApplication_create';
procedure QCoreApplication_destroy(handle: QCoreApplicationH); cdecl; external QtIntf name 'QCoreApplication_destroy'; 
procedure QCoreApplication_arguments(retval: QStringListH); cdecl; external QtIntf name 'QCoreApplication_arguments';
procedure QCoreApplication_setAttribute(attribute: QtApplicationAttribute; _on: Boolean = True); cdecl; external QtIntf name 'QCoreApplication_setAttribute';
function QCoreApplication_testAttribute(attribute: QtApplicationAttribute): Boolean; cdecl; external QtIntf name 'QCoreApplication_testAttribute';
procedure QCoreApplication_setOrganizationDomain(orgDomain: PWideString); cdecl; external QtIntf name 'QCoreApplication_setOrganizationDomain';
procedure QCoreApplication_organizationDomain(retval: PWideString); cdecl; external QtIntf name 'QCoreApplication_organizationDomain';
procedure QCoreApplication_setOrganizationName(orgName: PWideString); cdecl; external QtIntf name 'QCoreApplication_setOrganizationName';
procedure QCoreApplication_organizationName(retval: PWideString); cdecl; external QtIntf name 'QCoreApplication_organizationName';
procedure QCoreApplication_setApplicationName(application: PWideString); cdecl; external QtIntf name 'QCoreApplication_setApplicationName';
procedure QCoreApplication_applicationName(retval: PWideString); cdecl; external QtIntf name 'QCoreApplication_applicationName';
function QCoreApplication_instance(): QCoreApplicationH; cdecl; external QtIntf name 'QCoreApplication_instance';
function QCoreApplication_exec(): Integer; cdecl; external QtIntf name 'QCoreApplication_exec';
procedure QCoreApplication_processEvents(flags: QEventLoopProcessEventsFlags = QEventLoopAllEvents); overload; cdecl; external QtIntf name 'QCoreApplication_processEvents';
procedure QCoreApplication_processEvents(flags: QEventLoopProcessEventsFlags; maxtime: Integer); overload; cdecl; external QtIntf name 'QCoreApplication_processEvents2';
procedure QCoreApplication_exit(retcode: Integer = 0); cdecl; external QtIntf name 'QCoreApplication_exit';
function QCoreApplication_sendEvent(receiver: QObjectH; event: QEventH): Boolean; cdecl; external QtIntf name 'QCoreApplication_sendEvent';
procedure QCoreApplication_postEvent(receiver: QObjectH; event: QEventH); cdecl; external QtIntf name 'QCoreApplication_postEvent';
procedure QCoreApplication_sendPostedEvents(receiver: QObjectH; event_type: Integer); overload; cdecl; external QtIntf name 'QCoreApplication_sendPostedEvents';
procedure QCoreApplication_sendPostedEvents(); overload; cdecl; external QtIntf name 'QCoreApplication_sendPostedEvents2';
procedure QCoreApplication_removePostedEvents(receiver: QObjectH); cdecl; external QtIntf name 'QCoreApplication_removePostedEvents';
function QCoreApplication_hasPendingEvents(): Boolean; cdecl; external QtIntf name 'QCoreApplication_hasPendingEvents';
function QCoreApplication_notify(handle: QCoreApplicationH; p1: QObjectH; p2: QEventH): Boolean; cdecl; external QtIntf name 'QCoreApplication_notify';
function QCoreApplication_startingUp(): Boolean; cdecl; external QtIntf name 'QCoreApplication_startingUp';
function QCoreApplication_closingDown(): Boolean; cdecl; external QtIntf name 'QCoreApplication_closingDown';
procedure QCoreApplication_applicationDirPath(retval: PWideString); cdecl; external QtIntf name 'QCoreApplication_applicationDirPath';
procedure QCoreApplication_applicationFilePath(retval: PWideString); cdecl; external QtIntf name 'QCoreApplication_applicationFilePath';
procedure QCoreApplication_setLibraryPaths(p1: QStringListH); cdecl; external QtIntf name 'QCoreApplication_setLibraryPaths';
procedure QCoreApplication_libraryPaths(retval: QStringListH); cdecl; external QtIntf name 'QCoreApplication_libraryPaths';
procedure QCoreApplication_addLibraryPath(p1: PWideString); cdecl; external QtIntf name 'QCoreApplication_addLibraryPath';
procedure QCoreApplication_removeLibraryPath(p1: PWideString); cdecl; external QtIntf name 'QCoreApplication_removeLibraryPath';
procedure QCoreApplication_installTranslator(messageFile: QTranslatorH); cdecl; external QtIntf name 'QCoreApplication_installTranslator';
procedure QCoreApplication_removeTranslator(messageFile: QTranslatorH); cdecl; external QtIntf name 'QCoreApplication_removeTranslator';
procedure QCoreApplication_translate(retval: PWideString; context: PAnsiChar; key: PAnsiChar; comment: PAnsiChar = nil; encoding: QCoreApplicationEncoding = QCoreApplicationCodecForTr); overload; cdecl; external QtIntf name 'QCoreApplication_translate';
procedure QCoreApplication_translate(retval: PWideString; context: PAnsiChar; key: PAnsiChar; comment: PAnsiChar; encoding: QCoreApplicationEncoding; n: Integer); overload; cdecl; external QtIntf name 'QCoreApplication_translate2';
procedure QCoreApplication_flush(); cdecl; external QtIntf name 'QCoreApplication_flush';
{$ifdef UNIX or DARWIN }
procedure QCoreApplication_watchUnixSignal(signal: Integer; watch: Boolean); cdecl; external QtIntf name 'QCoreApplication_watchUnixSignal';
{$endif}
function QCoreApplication_setEventFilter(handle: QCoreApplicationH; filter: TCoreApplicationEventFilter): TCoreApplicationEventFilter; cdecl; external QtIntf name 'QCoreApplication_setEventFilter';
function QCoreApplication_filterEvent(handle: QCoreApplicationH; message: Pointer; result: PLong): Boolean; cdecl; external QtIntf name 'QCoreApplication_filterEvent';
procedure QCoreApplication_quit(); cdecl; external QtIntf name 'QCoreApplication_quit';
{$ifdef MSWINDOWS }
function QCoreApplication_winEventFilter(handle: QCoreApplicationH; message: PMsg; result: PLong): Boolean; cdecl; external QtIntf name 'QCoreApplication_winEventFilter';
{$endif}


type
  QCoreApplication_aboutToQuit_Event = procedure () of object cdecl;
  QCoreApplication_unixSignal_Event = procedure (p1: Integer) of object cdecl;


function QTranslator_create(parent: QObjectH = nil): QTranslatorH; cdecl; external QtIntf name 'QTranslator_create';
procedure QTranslator_destroy(handle: QTranslatorH); cdecl; external QtIntf name 'QTranslator_destroy'; 
procedure QTranslator_translate(handle: QTranslatorH; retval: PWideString; context: PAnsiChar; sourceText: PAnsiChar; comment: PAnsiChar = nil); overload; cdecl; external QtIntf name 'QTranslator_translate';
procedure QTranslator_translate(handle: QTranslatorH; retval: PWideString; context: PAnsiChar; sourceText: PAnsiChar; comment: PAnsiChar; n: Integer); overload; cdecl; external QtIntf name 'QTranslator_translate2';
function QTranslator_isEmpty(handle: QTranslatorH): Boolean; cdecl; external QtIntf name 'QTranslator_isEmpty';
function QTranslator_load(handle: QTranslatorH; filename: PWideString; directory: PWideString = nil; search_delimiters: PWideString = nil; suffix: PWideString = nil): Boolean; overload; cdecl; external QtIntf name 'QTranslator_load';
function QTranslator_load(handle: QTranslatorH; data: PByte; len: Integer): Boolean; overload; cdecl; external QtIntf name 'QTranslator_load2';

function QTimer_create(parent: QObjectH = nil): QTimerH; cdecl; external QtIntf name 'QTimer_create';
procedure QTimer_destroy(handle: QTimerH); cdecl; external QtIntf name 'QTimer_destroy'; 
function QTimer_isActive(handle: QTimerH): Boolean; cdecl; external QtIntf name 'QTimer_isActive';
function QTimer_timerId(handle: QTimerH): Integer; cdecl; external QtIntf name 'QTimer_timerId';
procedure QTimer_setInterval(handle: QTimerH; msec: Integer); cdecl; external QtIntf name 'QTimer_setInterval';
function QTimer_interval(handle: QTimerH): Integer; cdecl; external QtIntf name 'QTimer_interval';
procedure QTimer_setSingleShot(handle: QTimerH; singleShot: Boolean); cdecl; external QtIntf name 'QTimer_setSingleShot';
function QTimer_isSingleShot(handle: QTimerH): Boolean; cdecl; external QtIntf name 'QTimer_isSingleShot';
procedure QTimer_singleShot(msec: Integer; receiver: QObjectH; member: PAnsiChar); cdecl; external QtIntf name 'QTimer_singleShot';
procedure QTimer_start(handle: QTimerH; msec: Integer); overload; cdecl; external QtIntf name 'QTimer_start';
procedure QTimer_start(handle: QTimerH); overload; cdecl; external QtIntf name 'QTimer_start2';
procedure QTimer_stop(handle: QTimerH); cdecl; external QtIntf name 'QTimer_stop';


type
  QTimer_timeout_Event = procedure () of object cdecl;


type
  QVariantType = cardinal; //  QVariant::Type (4)

const
    QVariantInvalid = 0 { $0 };
    QVariantBool = 1 { $1 };
    QVariantInt = 2 { $2 };
    QVariantUInt = 3 { $3 };
    QVariantLongLong = 4 { $4 };
    QVariantULongLong = 5 { $5 };
    QVariantDouble = 6 { $6 };
    QVariantChar = 7 { $7 };
    QVariantMap = 8 { $8 };
    QVariantList = 9 { $9 };
    QVariantString = 10 { $a };
    QVariantStringList = 11 { $b };
    QVariantByteArray = 12 { $c };
    QVariantBitArray = 13 { $d };
    QVariantDate = 14 { $e };
    QVariantTime = 15 { $f };
    QVariantDateTime = 16 { $10 };
    QVariantUrl = 17 { $11 };
    QVariantLocale = 18 { $12 };
    QVariantRect = 19 { $13 };
    QVariantRectF = 20 { $14 };
    QVariantSize = 21 { $15 };
    QVariantSizeF = 22 { $16 };
    QVariantLine = 23 { $17 };
    QVariantLineF = 24 { $18 };
    QVariantPoint = 25 { $19 };
    QVariantPointF = 26 { $1a };
    QVariantRegExp = 27 { $1b };
    QVariantLastCoreType = 27 { $1b };
    QVariantFont = 64 { $40 };
    QVariantPixmap = 65 { $41 };
    QVariantBrush = 66 { $42 };
    QVariantColor = 67 { $43 };
    QVariantPalette = 68 { $44 };
    QVariantIcon = 69 { $45 };
    QVariantImage = 70 { $46 };
    QVariantPolygon = 71 { $47 };
    QVariantRegion = 72 { $48 };
    QVariantBitmap = 73 { $49 };
    QVariantCursor = 74 { $4a };
    QVariantSizePolicy = 75 { $4b };
    QVariantKeySequence = 76 { $4c };
    QVariantPen = 77 { $4d };
    QVariantTextLength = 78 { $4e };
    QVariantTextFormat = 79 { $4f };
    QVariantMatrix = 80 { $50 };
    QVariantLastGuiType = 80 { $50 };
    QVariantUserType = 127 { $7f };
    QVariantLastType = 4294967295 { $ffffffff };


function QVariant_create(): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create';
procedure QVariant_destroy(handle: QVariantH); cdecl; external QtIntf name 'QVariant_destroy'; 
function QVariant_create(_type: QVariantType): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create2';
function QVariant_create(typeOrUserType: Integer; copy: Pointer): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create3';
function QVariant_create(other: QVariantH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create4';
function QVariant_create(s: QDataStreamH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create5';
function QVariant_create(i: Integer): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create6';
function QVariant_create(ll: int64): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create8';
function QVariant_create(ull: qword): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create9';
function QVariant_create(b: Boolean): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create10';
function QVariant_create(d: Double): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create11';
function QVariant_create(str: PAnsiChar): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create12';
function QVariant_create(bytearray: QByteArrayH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create13';
function QVariant_create(bitarray: QBitArrayH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create14';
function QVariant_create(_string: PWideString): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create15';
function QVariant_create(_string: QLatin1StringH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create16';
function QVariant_create(stringlist: QStringListH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create17';
function QVariant_create(qchar: PWideChar): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create18';
function QVariant_create(date: QDateH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create19';
function QVariant_create(time: QTimeH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create20';
function QVariant_create(datetime: QDateTimeH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create21';
function QVariant_create(size: PSize): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create24';
function QVariant_create(size: QSizeFH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create25';
function QVariant_create(pt: PQtPoint): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create26';
function QVariant_create(pt: QPointFH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create27';
function QVariant_create(line: QLineH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create28';
function QVariant_create(line: QLineFH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create29';
function QVariant_create(rect: PRect): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create30';
function QVariant_create(rect: QRectFH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create31';
function QVariant_create(url: QUrlH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create32';
function QVariant_create(locale: QLocaleH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create33';
function QVariant_create(regExp: QRegExpH): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create34';
function QVariant_create(color: QtGlobalColor): QVariantH; overload; cdecl; external QtIntf name 'QVariant_create35';
function QVariant_type(handle: QVariantH): QVariantType; cdecl; external QtIntf name 'QVariant_type';
function QVariant_userType(handle: QVariantH): Integer; cdecl; external QtIntf name 'QVariant_userType';
function QVariant_typeName(handle: QVariantH): PAnsiChar; cdecl; external QtIntf name 'QVariant_typeName';
function QVariant_canConvert(handle: QVariantH; t: QVariantType): Boolean; overload; cdecl; external QtIntf name 'QVariant_canConvert';
function QVariant_convert(handle: QVariantH; t: QVariantType): Boolean; cdecl; external QtIntf name 'QVariant_convert';
function QVariant_isValid(handle: QVariantH): Boolean; cdecl; external QtIntf name 'QVariant_isValid';
function QVariant_isNull(handle: QVariantH): Boolean; cdecl; external QtIntf name 'QVariant_isNull';
procedure QVariant_clear(handle: QVariantH); cdecl; external QtIntf name 'QVariant_clear';
procedure QVariant_detach(handle: QVariantH); cdecl; external QtIntf name 'QVariant_detach';
function QVariant_isDetached(handle: QVariantH): Boolean; cdecl; external QtIntf name 'QVariant_isDetached';
function QVariant_toInt(handle: QVariantH; ok: PBoolean = nil): Integer; cdecl; external QtIntf name 'QVariant_toInt';
function QVariant_toUInt(handle: QVariantH; ok: PBoolean = nil): LongWord; cdecl; external QtIntf name 'QVariant_toUInt';
function QVariant_toLongLong(handle: QVariantH; ok: PBoolean = nil): int64; cdecl; external QtIntf name 'QVariant_toLongLong';
function QVariant_toULongLong(handle: QVariantH; ok: PBoolean = nil): qword; cdecl; external QtIntf name 'QVariant_toULongLong';
function QVariant_toBool(handle: QVariantH): Boolean; cdecl; external QtIntf name 'QVariant_toBool';
function QVariant_toDouble(handle: QVariantH; ok: PBoolean = nil): Double; cdecl; external QtIntf name 'QVariant_toDouble';
procedure QVariant_toByteArray(handle: QVariantH; retval: QByteArrayH); cdecl; external QtIntf name 'QVariant_toByteArray';
procedure QVariant_toBitArray(handle: QVariantH; retval: QBitArrayH); cdecl; external QtIntf name 'QVariant_toBitArray';
procedure QVariant_toString(handle: QVariantH; retval: PWideString); cdecl; external QtIntf name 'QVariant_toString';
procedure QVariant_toStringList(handle: QVariantH; retval: QStringListH); cdecl; external QtIntf name 'QVariant_toStringList';
procedure QVariant_toChar(handle: QVariantH; retval: PWideChar); cdecl; external QtIntf name 'QVariant_toChar';
procedure QVariant_toDate(handle: QVariantH; retval: QDateH); cdecl; external QtIntf name 'QVariant_toDate';
procedure QVariant_toTime(handle: QVariantH; retval: QTimeH); cdecl; external QtIntf name 'QVariant_toTime';
procedure QVariant_toDateTime(handle: QVariantH; retval: QDateTimeH); cdecl; external QtIntf name 'QVariant_toDateTime';
procedure QVariant_toPoint(handle: QVariantH; retval: PQtPoint); cdecl; external QtIntf name 'QVariant_toPoint';
procedure QVariant_toPointF(handle: QVariantH; retval: QPointFH); cdecl; external QtIntf name 'QVariant_toPointF';
procedure QVariant_toRect(handle: QVariantH; retval: PRect); cdecl; external QtIntf name 'QVariant_toRect';
procedure QVariant_toSize(handle: QVariantH; retval: PSize); cdecl; external QtIntf name 'QVariant_toSize';
procedure QVariant_toSizeF(handle: QVariantH; retval: QSizeFH); cdecl; external QtIntf name 'QVariant_toSizeF';
procedure QVariant_toLine(handle: QVariantH; retval: QLineH); cdecl; external QtIntf name 'QVariant_toLine';
procedure QVariant_toLineF(handle: QVariantH; retval: QLineFH); cdecl; external QtIntf name 'QVariant_toLineF';
procedure QVariant_toRectF(handle: QVariantH; retval: QRectFH); cdecl; external QtIntf name 'QVariant_toRectF';
procedure QVariant_toUrl(handle: QVariantH; retval: QUrlH); cdecl; external QtIntf name 'QVariant_toUrl';
procedure QVariant_toLocale(handle: QVariantH; retval: QLocaleH); cdecl; external QtIntf name 'QVariant_toLocale';
procedure QVariant_toRegExp(handle: QVariantH; retval: QRegExpH); cdecl; external QtIntf name 'QVariant_toRegExp';
procedure QVariant_load(handle: QVariantH; ds: QDataStreamH); cdecl; external QtIntf name 'QVariant_load';
procedure QVariant_save(handle: QVariantH; ds: QDataStreamH); cdecl; external QtIntf name 'QVariant_save';
function QVariant_typeToName(_type: QVariantType): PAnsiChar; cdecl; external QtIntf name 'QVariant_typeToName';
function QVariant_nameToType(name: PAnsiChar): QVariantType; cdecl; external QtIntf name 'QVariant_nameToType';
function QVariant_constData(handle: QVariantH): Pointer; cdecl; external QtIntf name 'QVariant_constData';


type
  QMetaMethodAccess = ( // QMetaMethod::Access (1)
    QMetaMethodPrivate, QMetaMethodProtected, QMetaMethodPublic );

  QMetaMethodMethodType = ( // QMetaMethod::MethodType (1)
    QMetaMethodMethod, QMetaMethodSignal, QMetaMethodSlot );

  QMetaMethodAttributes = (  //QMetaMethod::Attributes (2s)
    QMetaMethodCompatibility = $1,
    QMetaMethodCloned = $2,
    QMetaMethodScriptable = $4 );

function QMetaMethod_create(): QMetaMethodH; cdecl; external QtIntf name 'QMetaMethod_create';
procedure QMetaMethod_destroy(handle: QMetaMethodH); cdecl; external QtIntf name 'QMetaMethod_destroy'; 
function QMetaMethod_signature(handle: QMetaMethodH): PAnsiChar; cdecl; external QtIntf name 'QMetaMethod_signature';
function QMetaMethod_typeName(handle: QMetaMethodH): PAnsiChar; cdecl; external QtIntf name 'QMetaMethod_typeName';
function QMetaMethod_tag(handle: QMetaMethodH): PAnsiChar; cdecl; external QtIntf name 'QMetaMethod_tag';
function QMetaMethod_access(handle: QMetaMethodH): QMetaMethodAccess; cdecl; external QtIntf name 'QMetaMethod_access';
function QMetaMethod_methodType(handle: QMetaMethodH): QMetaMethodMethodType; cdecl; external QtIntf name 'QMetaMethod_methodType';
function QMetaMethod_attributes(handle: QMetaMethodH): Integer; cdecl; external QtIntf name 'QMetaMethod_attributes';

function QMetaEnum_create(): QMetaEnumH; cdecl; external QtIntf name 'QMetaEnum_create';
procedure QMetaEnum_destroy(handle: QMetaEnumH); cdecl; external QtIntf name 'QMetaEnum_destroy'; 
function QMetaEnum_name(handle: QMetaEnumH): PAnsiChar; cdecl; external QtIntf name 'QMetaEnum_name';
function QMetaEnum_isFlag(handle: QMetaEnumH): Boolean; cdecl; external QtIntf name 'QMetaEnum_isFlag';
function QMetaEnum_keyCount(handle: QMetaEnumH): Integer; cdecl; external QtIntf name 'QMetaEnum_keyCount';
function QMetaEnum_key(handle: QMetaEnumH; index: Integer): PAnsiChar; cdecl; external QtIntf name 'QMetaEnum_key';
function QMetaEnum_value(handle: QMetaEnumH; index: Integer): Integer; cdecl; external QtIntf name 'QMetaEnum_value';
function QMetaEnum_scope(handle: QMetaEnumH): PAnsiChar; cdecl; external QtIntf name 'QMetaEnum_scope';
function QMetaEnum_keyToValue(handle: QMetaEnumH; key: PAnsiChar): Integer; cdecl; external QtIntf name 'QMetaEnum_keyToValue';
function QMetaEnum_valueToKey(handle: QMetaEnumH; value: Integer): PAnsiChar; cdecl; external QtIntf name 'QMetaEnum_valueToKey';
function QMetaEnum_keysToValue(handle: QMetaEnumH; keys: PAnsiChar): Integer; cdecl; external QtIntf name 'QMetaEnum_keysToValue';
procedure QMetaEnum_valueToKeys(handle: QMetaEnumH; retval: QByteArrayH; value: Integer); cdecl; external QtIntf name 'QMetaEnum_valueToKeys';
function QMetaEnum_isValid(handle: QMetaEnumH): Boolean; cdecl; external QtIntf name 'QMetaEnum_isValid';

function QMetaProperty_create(): QMetaPropertyH; cdecl; external QtIntf name 'QMetaProperty_create';
procedure QMetaProperty_destroy(handle: QMetaPropertyH); cdecl; external QtIntf name 'QMetaProperty_destroy'; 
function QMetaProperty_name(handle: QMetaPropertyH): PAnsiChar; cdecl; external QtIntf name 'QMetaProperty_name';
function QMetaProperty_typeName(handle: QMetaPropertyH): PAnsiChar; cdecl; external QtIntf name 'QMetaProperty_typeName';
function QMetaProperty_type(handle: QMetaPropertyH): QVariantType; cdecl; external QtIntf name 'QMetaProperty_type';
function QMetaProperty_userType(handle: QMetaPropertyH): Integer; cdecl; external QtIntf name 'QMetaProperty_userType';
function QMetaProperty_isReadable(handle: QMetaPropertyH): Boolean; cdecl; external QtIntf name 'QMetaProperty_isReadable';
function QMetaProperty_isWritable(handle: QMetaPropertyH): Boolean; cdecl; external QtIntf name 'QMetaProperty_isWritable';
function QMetaProperty_isResettable(handle: QMetaPropertyH): Boolean; cdecl; external QtIntf name 'QMetaProperty_isResettable';
function QMetaProperty_isDesignable(handle: QMetaPropertyH; obj: QObjectH = nil): Boolean; cdecl; external QtIntf name 'QMetaProperty_isDesignable';
function QMetaProperty_isScriptable(handle: QMetaPropertyH; obj: QObjectH = nil): Boolean; cdecl; external QtIntf name 'QMetaProperty_isScriptable';
function QMetaProperty_isStored(handle: QMetaPropertyH; obj: QObjectH = nil): Boolean; cdecl; external QtIntf name 'QMetaProperty_isStored';
function QMetaProperty_isEditable(handle: QMetaPropertyH; obj: QObjectH = nil): Boolean; cdecl; external QtIntf name 'QMetaProperty_isEditable';
function QMetaProperty_isUser(handle: QMetaPropertyH; obj: QObjectH = nil): Boolean; cdecl; external QtIntf name 'QMetaProperty_isUser';
function QMetaProperty_isFlagType(handle: QMetaPropertyH): Boolean; cdecl; external QtIntf name 'QMetaProperty_isFlagType';
function QMetaProperty_isEnumType(handle: QMetaPropertyH): Boolean; cdecl; external QtIntf name 'QMetaProperty_isEnumType';
procedure QMetaProperty_enumerator(handle: QMetaPropertyH; retval: QMetaEnumH); cdecl; external QtIntf name 'QMetaProperty_enumerator';
procedure QMetaProperty_read(handle: QMetaPropertyH; retval: QVariantH; obj: QObjectH); cdecl; external QtIntf name 'QMetaProperty_read';
function QMetaProperty_write(handle: QMetaPropertyH; obj: QObjectH; value: QVariantH): Boolean; cdecl; external QtIntf name 'QMetaProperty_write';
function QMetaProperty_reset(handle: QMetaPropertyH; obj: QObjectH): Boolean; cdecl; external QtIntf name 'QMetaProperty_reset';
function QMetaProperty_hasStdCppSet(handle: QMetaPropertyH): Boolean; cdecl; external QtIntf name 'QMetaProperty_hasStdCppSet';
function QMetaProperty_isValid(handle: QMetaPropertyH): Boolean; cdecl; external QtIntf name 'QMetaProperty_isValid';

function QMetaClassInfo_create(): QMetaClassInfoH; cdecl; external QtIntf name 'QMetaClassInfo_create';
procedure QMetaClassInfo_destroy(handle: QMetaClassInfoH); cdecl; external QtIntf name 'QMetaClassInfo_destroy'; 
function QMetaClassInfo_name(handle: QMetaClassInfoH): PAnsiChar; cdecl; external QtIntf name 'QMetaClassInfo_name';
function QMetaClassInfo_value(handle: QMetaClassInfoH): PAnsiChar; cdecl; external QtIntf name 'QMetaClassInfo_value';

function QModelIndex_create(): QModelIndexH; overload; cdecl; external QtIntf name 'QModelIndex_create';
procedure QModelIndex_destroy(handle: QModelIndexH); cdecl; external QtIntf name 'QModelIndex_destroy'; 
function QModelIndex_create(other: QModelIndexH): QModelIndexH; overload; cdecl; external QtIntf name 'QModelIndex_create2';
function QModelIndex_row(handle: QModelIndexH): Integer; cdecl; external QtIntf name 'QModelIndex_row';
function QModelIndex_column(handle: QModelIndexH): Integer; cdecl; external QtIntf name 'QModelIndex_column';
function QModelIndex_internalPointer(handle: QModelIndexH): Pointer; cdecl; external QtIntf name 'QModelIndex_internalPointer';
function QModelIndex_internalId(handle: QModelIndexH): int64; cdecl; external QtIntf name 'QModelIndex_internalId';
procedure QModelIndex_parent(handle: QModelIndexH; retval: QModelIndexH); cdecl; external QtIntf name 'QModelIndex_parent';
procedure QModelIndex_sibling(handle: QModelIndexH; retval: QModelIndexH; row: Integer; column: Integer); cdecl; external QtIntf name 'QModelIndex_sibling';
procedure QModelIndex_child(handle: QModelIndexH; retval: QModelIndexH; row: Integer; column: Integer); cdecl; external QtIntf name 'QModelIndex_child';
procedure QModelIndex_data(handle: QModelIndexH; retval: QVariantH; role: QtItemDataRole = QtDisplayRole); cdecl; external QtIntf name 'QModelIndex_data';
function QModelIndex_flags(handle: QModelIndexH): QtItemFlags; cdecl; external QtIntf name 'QModelIndex_flags';
function QModelIndex_model(handle: QModelIndexH): QAbstractItemModelH; cdecl; external QtIntf name 'QModelIndex_model';
function QModelIndex_isValid(handle: QModelIndexH): Boolean; cdecl; external QtIntf name 'QModelIndex_isValid';

function QPersistentModelIndex_create(): QPersistentModelIndexH; overload; cdecl; external QtIntf name 'QPersistentModelIndex_create';
procedure QPersistentModelIndex_destroy(handle: QPersistentModelIndexH); cdecl; external QtIntf name 'QPersistentModelIndex_destroy'; 
function QPersistentModelIndex_create(index: QModelIndexH): QPersistentModelIndexH; overload; cdecl; external QtIntf name 'QPersistentModelIndex_create2';
function QPersistentModelIndex_create(other: QPersistentModelIndexH): QPersistentModelIndexH; overload; cdecl; external QtIntf name 'QPersistentModelIndex_create3';
function QPersistentModelIndex_row(handle: QPersistentModelIndexH): Integer; cdecl; external QtIntf name 'QPersistentModelIndex_row';
function QPersistentModelIndex_column(handle: QPersistentModelIndexH): Integer; cdecl; external QtIntf name 'QPersistentModelIndex_column';
function QPersistentModelIndex_internalPointer(handle: QPersistentModelIndexH): Pointer; cdecl; external QtIntf name 'QPersistentModelIndex_internalPointer';
function QPersistentModelIndex_internalId(handle: QPersistentModelIndexH): int64; cdecl; external QtIntf name 'QPersistentModelIndex_internalId';
procedure QPersistentModelIndex_parent(handle: QPersistentModelIndexH; retval: QModelIndexH); cdecl; external QtIntf name 'QPersistentModelIndex_parent';
procedure QPersistentModelIndex_sibling(handle: QPersistentModelIndexH; retval: QModelIndexH; row: Integer; column: Integer); cdecl; external QtIntf name 'QPersistentModelIndex_sibling';
procedure QPersistentModelIndex_child(handle: QPersistentModelIndexH; retval: QModelIndexH; row: Integer; column: Integer); cdecl; external QtIntf name 'QPersistentModelIndex_child';
procedure QPersistentModelIndex_data(handle: QPersistentModelIndexH; retval: QVariantH; role: QtItemDataRole = QtDisplayRole); cdecl; external QtIntf name 'QPersistentModelIndex_data';
function QPersistentModelIndex_flags(handle: QPersistentModelIndexH): QtItemFlags; cdecl; external QtIntf name 'QPersistentModelIndex_flags';
function QPersistentModelIndex_model(handle: QPersistentModelIndexH): QAbstractItemModelH; cdecl; external QtIntf name 'QPersistentModelIndex_model';
function QPersistentModelIndex_isValid(handle: QPersistentModelIndexH): Boolean; cdecl; external QtIntf name 'QPersistentModelIndex_isValid';

function QAbstractItemModel_hasIndex(handle: QAbstractItemModelH; row: Integer; column: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_hasIndex';
procedure QAbstractItemModel_index(handle: QAbstractItemModelH; retval: QModelIndexH; row: Integer; column: Integer; parent: QModelIndexH = nil); cdecl; external QtIntf name 'QAbstractItemModel_index';
procedure QAbstractItemModel_parent(handle: QAbstractItemModelH; retval: QModelIndexH; child: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemModel_parent';
procedure QAbstractItemModel_sibling(handle: QAbstractItemModelH; retval: QModelIndexH; row: Integer; column: Integer; idx: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemModel_sibling';
function QAbstractItemModel_rowCount(handle: QAbstractItemModelH; parent: QModelIndexH = nil): Integer; cdecl; external QtIntf name 'QAbstractItemModel_rowCount';
function QAbstractItemModel_columnCount(handle: QAbstractItemModelH; parent: QModelIndexH = nil): Integer; cdecl; external QtIntf name 'QAbstractItemModel_columnCount';
function QAbstractItemModel_hasChildren(handle: QAbstractItemModelH; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_hasChildren';
procedure QAbstractItemModel_data(handle: QAbstractItemModelH; retval: QVariantH; index: QModelIndexH; role: QtItemDataRole = QtDisplayRole); cdecl; external QtIntf name 'QAbstractItemModel_data';
function QAbstractItemModel_setData(handle: QAbstractItemModelH; index: QModelIndexH; value: QVariantH; role: QtItemDataRole = QtEditRole): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_setData';
procedure QAbstractItemModel_headerData(handle: QAbstractItemModelH; retval: QVariantH; section: Integer; orientation: QtOrientation; role: QtItemDataRole = QtDisplayRole); cdecl; external QtIntf name 'QAbstractItemModel_headerData';
function QAbstractItemModel_setHeaderData(handle: QAbstractItemModelH; section: Integer; orientation: QtOrientation; value: QVariantH; role: QtItemDataRole = QtEditRole): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_setHeaderData';
procedure QAbstractItemModel_mimeTypes(handle: QAbstractItemModelH; retval: QStringListH); cdecl; external QtIntf name 'QAbstractItemModel_mimeTypes';
function QAbstractItemModel_dropMimeData(handle: QAbstractItemModelH; data: QMimeDataH; action: QtDropAction; row: Integer; column: Integer; parent: QModelIndexH): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_dropMimeData';
function QAbstractItemModel_supportedDropActions(handle: QAbstractItemModelH): QtDropActions; cdecl; external QtIntf name 'QAbstractItemModel_supportedDropActions';
function QAbstractItemModel_supportedDragActions(handle: QAbstractItemModelH): QtDropActions; cdecl; external QtIntf name 'QAbstractItemModel_supportedDragActions';
procedure QAbstractItemModel_setSupportedDragActions(handle: QAbstractItemModelH; p1: QtDropActions); cdecl; external QtIntf name 'QAbstractItemModel_setSupportedDragActions';
function QAbstractItemModel_insertRows(handle: QAbstractItemModelH; row: Integer; count: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_insertRows';
function QAbstractItemModel_insertColumns(handle: QAbstractItemModelH; column: Integer; count: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_insertColumns';
function QAbstractItemModel_removeRows(handle: QAbstractItemModelH; row: Integer; count: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_removeRows';
function QAbstractItemModel_removeColumns(handle: QAbstractItemModelH; column: Integer; count: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_removeColumns';
function QAbstractItemModel_insertRow(handle: QAbstractItemModelH; row: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_insertRow';
function QAbstractItemModel_insertColumn(handle: QAbstractItemModelH; column: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_insertColumn';
function QAbstractItemModel_removeRow(handle: QAbstractItemModelH; row: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_removeRow';
function QAbstractItemModel_removeColumn(handle: QAbstractItemModelH; column: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_removeColumn';
procedure QAbstractItemModel_fetchMore(handle: QAbstractItemModelH; parent: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemModel_fetchMore';
function QAbstractItemModel_canFetchMore(handle: QAbstractItemModelH; parent: QModelIndexH): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_canFetchMore';
function QAbstractItemModel_flags(handle: QAbstractItemModelH; index: QModelIndexH): QtItemFlags; cdecl; external QtIntf name 'QAbstractItemModel_flags';
procedure QAbstractItemModel_sort(handle: QAbstractItemModelH; column: Integer; order: QtSortOrder = QtAscendingOrder); cdecl; external QtIntf name 'QAbstractItemModel_sort';
procedure QAbstractItemModel_buddy(handle: QAbstractItemModelH; retval: QModelIndexH; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemModel_buddy';
procedure QAbstractItemModel_span(handle: QAbstractItemModelH; retval: PSize; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemModel_span';
function QAbstractItemModel_submit(handle: QAbstractItemModelH): Boolean; cdecl; external QtIntf name 'QAbstractItemModel_submit';
procedure QAbstractItemModel_revert(handle: QAbstractItemModelH); cdecl; external QtIntf name 'QAbstractItemModel_revert';

procedure QAbstractTableModel_index(handle: QAbstractTableModelH; retval: QModelIndexH; row: Integer; column: Integer; parent: QModelIndexH = nil); cdecl; external QtIntf name 'QAbstractTableModel_index';
function QAbstractTableModel_dropMimeData(handle: QAbstractTableModelH; data: QMimeDataH; action: QtDropAction; row: Integer; column: Integer; parent: QModelIndexH): Boolean; cdecl; external QtIntf name 'QAbstractTableModel_dropMimeData';

procedure QAbstractListModel_index(handle: QAbstractListModelH; retval: QModelIndexH; row: Integer; column: Integer = 0; parent: QModelIndexH = nil); cdecl; external QtIntf name 'QAbstractListModel_index';
function QAbstractListModel_dropMimeData(handle: QAbstractListModelH; data: QMimeDataH; action: QtDropAction; row: Integer; column: Integer; parent: QModelIndexH): Boolean; cdecl; external QtIntf name 'QAbstractListModel_dropMimeData';


type
  QAbstractItemModel_dataChanged_Event = procedure (topLeft: QModelIndexH; bottomRight: QModelIndexH) of object cdecl;
  QAbstractItemModel_headerDataChanged_Event = procedure (orientation: QtOrientation; first: Integer; last: Integer) of object cdecl;
  QAbstractItemModel_layoutChanged_Event = procedure () of object cdecl;
  QAbstractItemModel_layoutAboutToBeChanged_Event = procedure () of object cdecl;
  QAbstractItemModel_rowsAboutToBeInserted_Event = procedure (parent: QModelIndexH; first: Integer; last: Integer) of object cdecl;
  QAbstractItemModel_rowsInserted_Event = procedure (parent: QModelIndexH; first: Integer; last: Integer) of object cdecl;
  QAbstractItemModel_rowsAboutToBeRemoved_Event = procedure (parent: QModelIndexH; first: Integer; last: Integer) of object cdecl;
  QAbstractItemModel_rowsRemoved_Event = procedure (parent: QModelIndexH; first: Integer; last: Integer) of object cdecl;
  QAbstractItemModel_columnsAboutToBeInserted_Event = procedure (parent: QModelIndexH; first: Integer; last: Integer) of object cdecl;
  QAbstractItemModel_columnsInserted_Event = procedure (parent: QModelIndexH; first: Integer; last: Integer) of object cdecl;
  QAbstractItemModel_columnsAboutToBeRemoved_Event = procedure (parent: QModelIndexH; first: Integer; last: Integer) of object cdecl;
  QAbstractItemModel_columnsRemoved_Event = procedure (parent: QModelIndexH; first: Integer; last: Integer) of object cdecl;
  QAbstractItemModel_modelAboutToBeReset_Event = procedure () of object cdecl;
  QAbstractItemModel_modelReset_Event = procedure () of object cdecl;


function QAbstractEventDispatcher_instance(thread: QThreadH = nil): QAbstractEventDispatcherH; cdecl; external QtIntf name 'QAbstractEventDispatcher_instance';
function QAbstractEventDispatcher_processEvents(handle: QAbstractEventDispatcherH; flags: QEventLoopProcessEventsFlags): Boolean; cdecl; external QtIntf name 'QAbstractEventDispatcher_processEvents';
function QAbstractEventDispatcher_hasPendingEvents(handle: QAbstractEventDispatcherH): Boolean; cdecl; external QtIntf name 'QAbstractEventDispatcher_hasPendingEvents';
procedure QAbstractEventDispatcher_registerSocketNotifier(handle: QAbstractEventDispatcherH; notifier: QSocketNotifierH); cdecl; external QtIntf name 'QAbstractEventDispatcher_registerSocketNotifier';
procedure QAbstractEventDispatcher_unregisterSocketNotifier(handle: QAbstractEventDispatcherH; notifier: QSocketNotifierH); cdecl; external QtIntf name 'QAbstractEventDispatcher_unregisterSocketNotifier';
function QAbstractEventDispatcher_registerTimer(handle: QAbstractEventDispatcherH; interval: Integer; _object: QObjectH): Integer; overload; cdecl; external QtIntf name 'QAbstractEventDispatcher_registerTimer';
procedure QAbstractEventDispatcher_registerTimer(handle: QAbstractEventDispatcherH; timerId: Integer; interval: Integer; _object: QObjectH); overload; cdecl; external QtIntf name 'QAbstractEventDispatcher_registerTimer2';
function QAbstractEventDispatcher_unregisterTimer(handle: QAbstractEventDispatcherH; timerId: Integer): Boolean; cdecl; external QtIntf name 'QAbstractEventDispatcher_unregisterTimer';
function QAbstractEventDispatcher_unregisterTimers(handle: QAbstractEventDispatcherH; _object: QObjectH): Boolean; cdecl; external QtIntf name 'QAbstractEventDispatcher_unregisterTimers';
procedure QAbstractEventDispatcher_wakeUp(handle: QAbstractEventDispatcherH); cdecl; external QtIntf name 'QAbstractEventDispatcher_wakeUp';
procedure QAbstractEventDispatcher_interrupt(handle: QAbstractEventDispatcherH); cdecl; external QtIntf name 'QAbstractEventDispatcher_interrupt';
procedure QAbstractEventDispatcher_flush(handle: QAbstractEventDispatcherH); cdecl; external QtIntf name 'QAbstractEventDispatcher_flush';
procedure QAbstractEventDispatcher_startingUp(handle: QAbstractEventDispatcherH); cdecl; external QtIntf name 'QAbstractEventDispatcher_startingUp';
procedure QAbstractEventDispatcher_closingDown(handle: QAbstractEventDispatcherH); cdecl; external QtIntf name 'QAbstractEventDispatcher_closingDown';
function QAbstractEventDispatcher_setEventFilter(handle: QAbstractEventDispatcherH; filter: TAbstractEventFilter): TAbstractEventFilter; cdecl; external QtIntf name 'QAbstractEventDispatcher_setEventFilter';
function QAbstractEventDispatcher_filterEvent(handle: QAbstractEventDispatcherH; message: Pointer): Boolean; cdecl; external QtIntf name 'QAbstractEventDispatcher_filterEvent';

function QMimeData_create(): QMimeDataH; cdecl; external QtIntf name 'QMimeData_create';
procedure QMimeData_destroy(handle: QMimeDataH); cdecl; external QtIntf name 'QMimeData_destroy'; 
function QMimeData_hasUrls(handle: QMimeDataH): Boolean; cdecl; external QtIntf name 'QMimeData_hasUrls';
procedure QMimeData_text(handle: QMimeDataH; retval: PWideString); cdecl; external QtIntf name 'QMimeData_text';
procedure QMimeData_setText(handle: QMimeDataH; text: PWideString); cdecl; external QtIntf name 'QMimeData_setText';
function QMimeData_hasText(handle: QMimeDataH): Boolean; cdecl; external QtIntf name 'QMimeData_hasText';
procedure QMimeData_html(handle: QMimeDataH; retval: PWideString); cdecl; external QtIntf name 'QMimeData_html';
procedure QMimeData_setHtml(handle: QMimeDataH; html: PWideString); cdecl; external QtIntf name 'QMimeData_setHtml';
function QMimeData_hasHtml(handle: QMimeDataH): Boolean; cdecl; external QtIntf name 'QMimeData_hasHtml';
procedure QMimeData_imageData(handle: QMimeDataH; retval: QVariantH); cdecl; external QtIntf name 'QMimeData_imageData';
procedure QMimeData_setImageData(handle: QMimeDataH; image: QVariantH); cdecl; external QtIntf name 'QMimeData_setImageData';
function QMimeData_hasImage(handle: QMimeDataH): Boolean; cdecl; external QtIntf name 'QMimeData_hasImage';
procedure QMimeData_colorData(handle: QMimeDataH; retval: QVariantH); cdecl; external QtIntf name 'QMimeData_colorData';
procedure QMimeData_setColorData(handle: QMimeDataH; color: QVariantH); cdecl; external QtIntf name 'QMimeData_setColorData';
function QMimeData_hasColor(handle: QMimeDataH): Boolean; cdecl; external QtIntf name 'QMimeData_hasColor';
procedure QMimeData_data(handle: QMimeDataH; retval: QByteArrayH; mimetype: PWideString); cdecl; external QtIntf name 'QMimeData_data';
procedure QMimeData_setData(handle: QMimeDataH; mimetype: PWideString; data: QByteArrayH); cdecl; external QtIntf name 'QMimeData_setData';
function QMimeData_hasFormat(handle: QMimeDataH; mimetype: PWideString): Boolean; cdecl; external QtIntf name 'QMimeData_hasFormat';
procedure QMimeData_formats(handle: QMimeDataH; retval: QStringListH); cdecl; external QtIntf name 'QMimeData_formats';
procedure QMimeData_clear(handle: QMimeDataH); cdecl; external QtIntf name 'QMimeData_clear';

function QStringList_create(): QStringListH; overload; cdecl; external QtIntf name 'QStringList_create';
procedure QStringList_destroy(handle: QStringListH); cdecl; external QtIntf name 'QStringList_destroy'; 
function QStringList_create(i: PWideString): QStringListH; overload; cdecl; external QtIntf name 'QStringList_create2';
function QStringList_create(l: QStringListH): QStringListH; overload; cdecl; external QtIntf name 'QStringList_create3';
procedure QStringList_sort(handle: QStringListH); cdecl; external QtIntf name 'QStringList_sort';
procedure QStringList_join(handle: QStringListH; retval: PWideString; sep: PWideString); cdecl; external QtIntf name 'QStringList_join';
procedure QStringList_filter(handle: QStringListH; retval: QStringListH; str: PWideString; cs: QtCaseSensitivity = QtCaseSensitive); overload; cdecl; external QtIntf name 'QStringList_filter';
function QStringList_contains(handle: QStringListH; str: PWideString; cs: QtCaseSensitivity = QtCaseSensitive): boolean; cdecl; external QtIntf name 'QStringList_contains';
function QStringList_replaceInStrings(handle: QStringListH; before: PWideString; after: PWideString; cs: QtCaseSensitivity = QtCaseSensitive): QStringListH; overload; cdecl; external QtIntf name 'QStringList_replaceInStrings';
procedure QStringList_filter(handle: QStringListH; retval: QStringListH; rx: QRegExpH); overload; cdecl; external QtIntf name 'QStringList_filter2';
function QStringList_replaceInStrings(handle: QStringListH; rx: QRegExpH; after: PWideString): QStringListH; overload; cdecl; external QtIntf name 'QStringList_replaceInStrings2';
function QStringList_indexOf(handle: QStringListH; rx: QRegExpH; from: Integer = 0): Integer; cdecl; external QtIntf name 'QStringList_indexOf';
function QStringList_lastIndexOf(handle: QStringListH; rx: QRegExpH; from: Integer = -1): Integer; cdecl; external QtIntf name 'QStringList_lastIndexOf';
function QStringList_size(handle: QStringListH): Integer; cdecl; external QtIntf name 'QStringList_size';
function QStringList_isEmpty(handle: QStringListH): Boolean; cdecl; external QtIntf name 'QStringList_isEmpty';
procedure QStringList_clear(handle: QStringListH); cdecl; external QtIntf name 'QStringList_clear';
procedure QStringList_at(handle: QStringListH; retval: PWideString; i: Integer); cdecl; external QtIntf name 'QStringList_at';
procedure QStringList_append(handle: QStringListH; s: PWideString); cdecl; external QtIntf name 'QStringList_append';
procedure QStringList_prepend(handle: QStringListH; s: PWideString); cdecl; external QtIntf name 'QStringList_prepend';
procedure QStringList_insert(handle: QStringListH; i: Integer; s: PWideString); cdecl; external QtIntf name 'QStringList_insert';
procedure QStringList_replace(handle: QStringListH; i: Integer; s: PWideString); cdecl; external QtIntf name 'QStringList_replace';
procedure QStringList_removeAt(handle: QStringListH; i: Integer); cdecl; external QtIntf name 'QStringList_removeAt';
function QStringList_removeAll(handle: QStringListH; s: PWideString): Integer; cdecl; external QtIntf name 'QStringList_removeAll';
procedure QStringList_takeAt(handle: QStringListH; retval: PWideString; i: Integer); cdecl; external QtIntf name 'QStringList_takeAt';
procedure QStringList_takeFirst(handle: QStringListH; retval: PWideString); cdecl; external QtIntf name 'QStringList_takeFirst';
procedure QStringList_takeLast(handle: QStringListH; retval: PWideString); cdecl; external QtIntf name 'QStringList_takeLast';
procedure QStringList_move(handle: QStringListH; from: Integer; _to: Integer); cdecl; external QtIntf name 'QStringList_move';
procedure QStringList_swap(handle: QStringListH; i: Integer; j: Integer); cdecl; external QtIntf name 'QStringList_swap';

function QRect_create(): QRectH; overload; cdecl; external QtIntf name 'QRect_create';
procedure QRect_destroy(handle: QRectH); cdecl; external QtIntf name 'QRect_destroy'; 
function QRect_create(topleft: PQtPoint; bottomright: PQtPoint): QRectH; overload; cdecl; external QtIntf name 'QRect_create2';
function QRect_create(topleft: PQtPoint; size: PSize): QRectH; overload; cdecl; external QtIntf name 'QRect_create3';
function QRect_create(left: Integer; top: Integer; width: Integer; height: Integer): QRectH; overload; cdecl; external QtIntf name 'QRect_create4';
function QRect_isNull(handle: QRectH): Boolean; cdecl; external QtIntf name 'QRect_isNull';
function QRect_isEmpty(handle: QRectH): Boolean; cdecl; external QtIntf name 'QRect_isEmpty';
function QRect_isValid(handle: QRectH): Boolean; cdecl; external QtIntf name 'QRect_isValid';
function QRect_left(handle: QRectH): Integer; cdecl; external QtIntf name 'QRect_left';
function QRect_top(handle: QRectH): Integer; cdecl; external QtIntf name 'QRect_top';
function QRect_right(handle: QRectH): Integer; cdecl; external QtIntf name 'QRect_right';
function QRect_bottom(handle: QRectH): Integer; cdecl; external QtIntf name 'QRect_bottom';
procedure QRect_normalized(handle: QRectH; retval: PRect); cdecl; external QtIntf name 'QRect_normalized';
function QRect_x(handle: QRectH): Integer; cdecl; external QtIntf name 'QRect_x';
function QRect_y(handle: QRectH): Integer; cdecl; external QtIntf name 'QRect_y';
procedure QRect_setLeft(handle: QRectH; pos: Integer); cdecl; external QtIntf name 'QRect_setLeft';
procedure QRect_setTop(handle: QRectH; pos: Integer); cdecl; external QtIntf name 'QRect_setTop';
procedure QRect_setRight(handle: QRectH; pos: Integer); cdecl; external QtIntf name 'QRect_setRight';
procedure QRect_setBottom(handle: QRectH; pos: Integer); cdecl; external QtIntf name 'QRect_setBottom';
procedure QRect_setX(handle: QRectH; x: Integer); cdecl; external QtIntf name 'QRect_setX';
procedure QRect_setY(handle: QRectH; y: Integer); cdecl; external QtIntf name 'QRect_setY';
procedure QRect_setTopLeft(handle: QRectH; p: PQtPoint); cdecl; external QtIntf name 'QRect_setTopLeft';
procedure QRect_setBottomRight(handle: QRectH; p: PQtPoint); cdecl; external QtIntf name 'QRect_setBottomRight';
procedure QRect_setTopRight(handle: QRectH; p: PQtPoint); cdecl; external QtIntf name 'QRect_setTopRight';
procedure QRect_setBottomLeft(handle: QRectH; p: PQtPoint); cdecl; external QtIntf name 'QRect_setBottomLeft';
procedure QRect_topLeft(handle: QRectH; retval: PQtPoint); cdecl; external QtIntf name 'QRect_topLeft';
procedure QRect_bottomRight(handle: QRectH; retval: PQtPoint); cdecl; external QtIntf name 'QRect_bottomRight';
procedure QRect_topRight(handle: QRectH; retval: PQtPoint); cdecl; external QtIntf name 'QRect_topRight';
procedure QRect_bottomLeft(handle: QRectH; retval: PQtPoint); cdecl; external QtIntf name 'QRect_bottomLeft';
procedure QRect_center(handle: QRectH; retval: PQtPoint); cdecl; external QtIntf name 'QRect_center';
procedure QRect_moveLeft(handle: QRectH; pos: Integer); cdecl; external QtIntf name 'QRect_moveLeft';
procedure QRect_moveTop(handle: QRectH; pos: Integer); cdecl; external QtIntf name 'QRect_moveTop';
procedure QRect_moveRight(handle: QRectH; pos: Integer); cdecl; external QtIntf name 'QRect_moveRight';
procedure QRect_moveBottom(handle: QRectH; pos: Integer); cdecl; external QtIntf name 'QRect_moveBottom';
procedure QRect_moveTopLeft(handle: QRectH; p: PQtPoint); cdecl; external QtIntf name 'QRect_moveTopLeft';
procedure QRect_moveBottomRight(handle: QRectH; p: PQtPoint); cdecl; external QtIntf name 'QRect_moveBottomRight';
procedure QRect_moveTopRight(handle: QRectH; p: PQtPoint); cdecl; external QtIntf name 'QRect_moveTopRight';
procedure QRect_moveBottomLeft(handle: QRectH; p: PQtPoint); cdecl; external QtIntf name 'QRect_moveBottomLeft';
procedure QRect_moveCenter(handle: QRectH; p: PQtPoint); cdecl; external QtIntf name 'QRect_moveCenter';
procedure QRect_translate(handle: QRectH; dx: Integer; dy: Integer); overload; cdecl; external QtIntf name 'QRect_translate';
procedure QRect_translate(handle: QRectH; p: PQtPoint); overload; cdecl; external QtIntf name 'QRect_translate2';
procedure QRect_translated(handle: QRectH; retval: PRect; dx: Integer; dy: Integer); overload; cdecl; external QtIntf name 'QRect_translated';
procedure QRect_translated(handle: QRectH; retval: PRect; p: PQtPoint); overload; cdecl; external QtIntf name 'QRect_translated2';
procedure QRect_moveTo(handle: QRectH; x: Integer; t: Integer); overload; cdecl; external QtIntf name 'QRect_moveTo';
procedure QRect_moveTo(handle: QRectH; p: PQtPoint); overload; cdecl; external QtIntf name 'QRect_moveTo2';
procedure QRect_setRect(handle: QRectH; x: Integer; y: Integer; w: Integer; h: Integer); cdecl; external QtIntf name 'QRect_setRect';
procedure QRect_getRect(handle: QRectH; x: PInteger; y: PInteger; w: PInteger; h: PInteger); cdecl; external QtIntf name 'QRect_getRect';
procedure QRect_setCoords(handle: QRectH; x1: Integer; y1: Integer; x2: Integer; y2: Integer); cdecl; external QtIntf name 'QRect_setCoords';
procedure QRect_getCoords(handle: QRectH; x1: PInteger; y1: PInteger; x2: PInteger; y2: PInteger); cdecl; external QtIntf name 'QRect_getCoords';
procedure QRect_adjust(handle: QRectH; x1: Integer; y1: Integer; x2: Integer; y2: Integer); cdecl; external QtIntf name 'QRect_adjust';
procedure QRect_adjusted(handle: QRectH; retval: PRect; x1: Integer; y1: Integer; x2: Integer; y2: Integer); cdecl; external QtIntf name 'QRect_adjusted';
procedure QRect_size(handle: QRectH; retval: PSize); cdecl; external QtIntf name 'QRect_size';
function QRect_width(handle: QRectH): Integer; cdecl; external QtIntf name 'QRect_width';
function QRect_height(handle: QRectH): Integer; cdecl; external QtIntf name 'QRect_height';
procedure QRect_setWidth(handle: QRectH; w: Integer); cdecl; external QtIntf name 'QRect_setWidth';
procedure QRect_setHeight(handle: QRectH; h: Integer); cdecl; external QtIntf name 'QRect_setHeight';
procedure QRect_setSize(handle: QRectH; s: PSize); cdecl; external QtIntf name 'QRect_setSize';
function QRect_contains(handle: QRectH; p: PQtPoint; proper: Boolean = False): Boolean; overload; cdecl; external QtIntf name 'QRect_contains';
function QRect_contains(handle: QRectH; x: Integer; y: Integer): Boolean; overload; cdecl; external QtIntf name 'QRect_contains2';
function QRect_contains(handle: QRectH; x: Integer; y: Integer; proper: Boolean): Boolean; overload; cdecl; external QtIntf name 'QRect_contains3';
function QRect_contains(handle: QRectH; r: PRect; proper: Boolean = False): Boolean; overload; cdecl; external QtIntf name 'QRect_contains4';
procedure QRect_unite(handle: QRectH; retval: PRect; r: PRect); cdecl; external QtIntf name 'QRect_unite';
procedure QRect_united(handle: QRectH; retval: PRect; other: PRect); cdecl; external QtIntf name 'QRect_united';
procedure QRect_intersect(handle: QRectH; retval: PRect; r: PRect); cdecl; external QtIntf name 'QRect_intersect';
procedure QRect_intersected(handle: QRectH; retval: PRect; other: PRect); cdecl; external QtIntf name 'QRect_intersected';
function QRect_intersects(handle: QRectH; r: PRect): Boolean; cdecl; external QtIntf name 'QRect_intersects';

function QRectF_create(): QRectFH; overload; cdecl; external QtIntf name 'QRectF_create';
procedure QRectF_destroy(handle: QRectFH); cdecl; external QtIntf name 'QRectF_destroy'; 
function QRectF_create(topleft: QPointFH; size: QSizeFH): QRectFH; overload; cdecl; external QtIntf name 'QRectF_create2';
function QRectF_create(left: Double; top: Double; width: Double; height: Double): QRectFH; overload; cdecl; external QtIntf name 'QRectF_create3';
function QRectF_create(rect: PRect): QRectFH; overload; cdecl; external QtIntf name 'QRectF_create4';
function QRectF_isNull(handle: QRectFH): Boolean; cdecl; external QtIntf name 'QRectF_isNull';
function QRectF_isEmpty(handle: QRectFH): Boolean; cdecl; external QtIntf name 'QRectF_isEmpty';
function QRectF_isValid(handle: QRectFH): Boolean; cdecl; external QtIntf name 'QRectF_isValid';
procedure QRectF_normalized(handle: QRectFH; retval: QRectFH); cdecl; external QtIntf name 'QRectF_normalized';
function QRectF_left(handle: QRectFH): Double; cdecl; external QtIntf name 'QRectF_left';
function QRectF_top(handle: QRectFH): Double; cdecl; external QtIntf name 'QRectF_top';
function QRectF_right(handle: QRectFH): Double; cdecl; external QtIntf name 'QRectF_right';
function QRectF_bottom(handle: QRectFH): Double; cdecl; external QtIntf name 'QRectF_bottom';
function QRectF_x(handle: QRectFH): Double; cdecl; external QtIntf name 'QRectF_x';
function QRectF_y(handle: QRectFH): Double; cdecl; external QtIntf name 'QRectF_y';
procedure QRectF_setLeft(handle: QRectFH; pos: Double); cdecl; external QtIntf name 'QRectF_setLeft';
procedure QRectF_setTop(handle: QRectFH; pos: Double); cdecl; external QtIntf name 'QRectF_setTop';
procedure QRectF_setRight(handle: QRectFH; pos: Double); cdecl; external QtIntf name 'QRectF_setRight';
procedure QRectF_setBottom(handle: QRectFH; pos: Double); cdecl; external QtIntf name 'QRectF_setBottom';
procedure QRectF_setX(handle: QRectFH; pos: Double); cdecl; external QtIntf name 'QRectF_setX';
procedure QRectF_setY(handle: QRectFH; pos: Double); cdecl; external QtIntf name 'QRectF_setY';
procedure QRectF_topLeft(handle: QRectFH; retval: QPointFH); cdecl; external QtIntf name 'QRectF_topLeft';
procedure QRectF_bottomRight(handle: QRectFH; retval: QPointFH); cdecl; external QtIntf name 'QRectF_bottomRight';
procedure QRectF_topRight(handle: QRectFH; retval: QPointFH); cdecl; external QtIntf name 'QRectF_topRight';
procedure QRectF_bottomLeft(handle: QRectFH; retval: QPointFH); cdecl; external QtIntf name 'QRectF_bottomLeft';
procedure QRectF_center(handle: QRectFH; retval: QPointFH); cdecl; external QtIntf name 'QRectF_center';
procedure QRectF_setTopLeft(handle: QRectFH; p: QPointFH); cdecl; external QtIntf name 'QRectF_setTopLeft';
procedure QRectF_setBottomRight(handle: QRectFH; p: QPointFH); cdecl; external QtIntf name 'QRectF_setBottomRight';
procedure QRectF_setTopRight(handle: QRectFH; p: QPointFH); cdecl; external QtIntf name 'QRectF_setTopRight';
procedure QRectF_setBottomLeft(handle: QRectFH; p: QPointFH); cdecl; external QtIntf name 'QRectF_setBottomLeft';
procedure QRectF_moveLeft(handle: QRectFH; pos: Double); cdecl; external QtIntf name 'QRectF_moveLeft';
procedure QRectF_moveTop(handle: QRectFH; pos: Double); cdecl; external QtIntf name 'QRectF_moveTop';
procedure QRectF_moveRight(handle: QRectFH; pos: Double); cdecl; external QtIntf name 'QRectF_moveRight';
procedure QRectF_moveBottom(handle: QRectFH; pos: Double); cdecl; external QtIntf name 'QRectF_moveBottom';
procedure QRectF_moveTopLeft(handle: QRectFH; p: QPointFH); cdecl; external QtIntf name 'QRectF_moveTopLeft';
procedure QRectF_moveBottomRight(handle: QRectFH; p: QPointFH); cdecl; external QtIntf name 'QRectF_moveBottomRight';
procedure QRectF_moveTopRight(handle: QRectFH; p: QPointFH); cdecl; external QtIntf name 'QRectF_moveTopRight';
procedure QRectF_moveBottomLeft(handle: QRectFH; p: QPointFH); cdecl; external QtIntf name 'QRectF_moveBottomLeft';
procedure QRectF_moveCenter(handle: QRectFH; p: QPointFH); cdecl; external QtIntf name 'QRectF_moveCenter';
procedure QRectF_translate(handle: QRectFH; dx: Double; dy: Double); overload; cdecl; external QtIntf name 'QRectF_translate';
procedure QRectF_translate(handle: QRectFH; p: QPointFH); overload; cdecl; external QtIntf name 'QRectF_translate2';
procedure QRectF_translated(handle: QRectFH; retval: QRectFH; dx: Double; dy: Double); overload; cdecl; external QtIntf name 'QRectF_translated';
procedure QRectF_translated(handle: QRectFH; retval: QRectFH; p: QPointFH); overload; cdecl; external QtIntf name 'QRectF_translated2';
procedure QRectF_moveTo(handle: QRectFH; x: Double; t: Double); overload; cdecl; external QtIntf name 'QRectF_moveTo';
procedure QRectF_moveTo(handle: QRectFH; p: QPointFH); overload; cdecl; external QtIntf name 'QRectF_moveTo2';
procedure QRectF_setRect(handle: QRectFH; x: Double; y: Double; w: Double; h: Double); cdecl; external QtIntf name 'QRectF_setRect';
procedure QRectF_getRect(handle: QRectFH; x: PDouble; y: PDouble; w: PDouble; h: PDouble); cdecl; external QtIntf name 'QRectF_getRect';
procedure QRectF_setCoords(handle: QRectFH; x1: Double; y1: Double; x2: Double; y2: Double); cdecl; external QtIntf name 'QRectF_setCoords';
procedure QRectF_getCoords(handle: QRectFH; x1: PDouble; y1: PDouble; x2: PDouble; y2: PDouble); cdecl; external QtIntf name 'QRectF_getCoords';
procedure QRectF_adjust(handle: QRectFH; x1: Double; y1: Double; x2: Double; y2: Double); cdecl; external QtIntf name 'QRectF_adjust';
procedure QRectF_adjusted(handle: QRectFH; retval: QRectFH; x1: Double; y1: Double; x2: Double; y2: Double); cdecl; external QtIntf name 'QRectF_adjusted';
procedure QRectF_size(handle: QRectFH; retval: QSizeFH); cdecl; external QtIntf name 'QRectF_size';
function QRectF_width(handle: QRectFH): Double; cdecl; external QtIntf name 'QRectF_width';
function QRectF_height(handle: QRectFH): Double; cdecl; external QtIntf name 'QRectF_height';
procedure QRectF_setWidth(handle: QRectFH; w: Double); cdecl; external QtIntf name 'QRectF_setWidth';
procedure QRectF_setHeight(handle: QRectFH; h: Double); cdecl; external QtIntf name 'QRectF_setHeight';
procedure QRectF_setSize(handle: QRectFH; s: QSizeFH); cdecl; external QtIntf name 'QRectF_setSize';
function QRectF_contains(handle: QRectFH; p: QPointFH): Boolean; overload; cdecl; external QtIntf name 'QRectF_contains';
function QRectF_contains(handle: QRectFH; x: Double; y: Double): Boolean; overload; cdecl; external QtIntf name 'QRectF_contains2';
function QRectF_contains(handle: QRectFH; r: QRectFH): Boolean; overload; cdecl; external QtIntf name 'QRectF_contains3';
procedure QRectF_unite(handle: QRectFH; retval: QRectFH; r: QRectFH); cdecl; external QtIntf name 'QRectF_unite';
procedure QRectF_united(handle: QRectFH; retval: QRectFH; other: QRectFH); cdecl; external QtIntf name 'QRectF_united';
procedure QRectF_intersect(handle: QRectFH; retval: QRectFH; r: QRectFH); cdecl; external QtIntf name 'QRectF_intersect';
procedure QRectF_intersected(handle: QRectFH; retval: QRectFH; other: QRectFH); cdecl; external QtIntf name 'QRectF_intersected';
function QRectF_intersects(handle: QRectFH; r: QRectFH): Boolean; cdecl; external QtIntf name 'QRectF_intersects';
procedure QRectF_toRect(handle: QRectFH; retval: PRect); cdecl; external QtIntf name 'QRectF_toRect';

function QDate_create(): QDateH; overload; cdecl; external QtIntf name 'QDate_create';
procedure QDate_destroy(handle: QDateH); cdecl; external QtIntf name 'QDate_destroy'; 
function QDate_create(y: Integer; m: Integer; d: Integer): QDateH; overload; cdecl; external QtIntf name 'QDate_create2';
function QDate_isNull(handle: QDateH): Boolean; cdecl; external QtIntf name 'QDate_isNull';
function QDate_isValid(handle: QDateH): Boolean; overload; cdecl; external QtIntf name 'QDate_isValid';
function QDate_year(handle: QDateH): Integer; cdecl; external QtIntf name 'QDate_year';
function QDate_month(handle: QDateH): Integer; cdecl; external QtIntf name 'QDate_month';
function QDate_day(handle: QDateH): Integer; cdecl; external QtIntf name 'QDate_day';
function QDate_dayOfWeek(handle: QDateH): Integer; cdecl; external QtIntf name 'QDate_dayOfWeek';
function QDate_dayOfYear(handle: QDateH): Integer; cdecl; external QtIntf name 'QDate_dayOfYear';
function QDate_daysInMonth(handle: QDateH): Integer; cdecl; external QtIntf name 'QDate_daysInMonth';
function QDate_daysInYear(handle: QDateH): Integer; cdecl; external QtIntf name 'QDate_daysInYear';
function QDate_weekNumber(handle: QDateH; yearNum: PInteger = nil): Integer; cdecl; external QtIntf name 'QDate_weekNumber';
procedure QDate_shortMonthName(retval: PWideString; month: Integer); cdecl; external QtIntf name 'QDate_shortMonthName';
procedure QDate_shortDayName(retval: PWideString; weekday: Integer); cdecl; external QtIntf name 'QDate_shortDayName';
procedure QDate_longMonthName(retval: PWideString; month: Integer); cdecl; external QtIntf name 'QDate_longMonthName';
procedure QDate_longDayName(retval: PWideString; weekday: Integer); cdecl; external QtIntf name 'QDate_longDayName';
procedure QDate_toString(handle: QDateH; retval: PWideString; f: QtDateFormat = QtTextDate); overload; cdecl; external QtIntf name 'QDate_toString';
procedure QDate_toString(handle: QDateH; retval: PWideString; format: PWideString); overload; cdecl; external QtIntf name 'QDate_toString2';
function QDate_setYMD(handle: QDateH; y: Integer; m: Integer; d: Integer): Boolean; cdecl; external QtIntf name 'QDate_setYMD';
function QDate_setDate(handle: QDateH; year: Integer; month: Integer; date: Integer): Boolean; cdecl; external QtIntf name 'QDate_setDate';
procedure QDate_addDays(handle: QDateH; retval: QDateH; days: Integer); cdecl; external QtIntf name 'QDate_addDays';
procedure QDate_addMonths(handle: QDateH; retval: QDateH; months: Integer); cdecl; external QtIntf name 'QDate_addMonths';
procedure QDate_addYears(handle: QDateH; retval: QDateH; years: Integer); cdecl; external QtIntf name 'QDate_addYears';
function QDate_daysTo(handle: QDateH; p1: QDateH): Integer; cdecl; external QtIntf name 'QDate_daysTo';
procedure QDate_currentDate(retval: QDateH); cdecl; external QtIntf name 'QDate_currentDate';
procedure QDate_fromString(retval: QDateH; s: PWideString; f: QtDateFormat = QtTextDate); overload; cdecl; external QtIntf name 'QDate_fromString';
procedure QDate_fromString(retval: QDateH; s: PWideString; format: PWideString); overload; cdecl; external QtIntf name 'QDate_fromString2';
function QDate_isValid(y: Integer; m: Integer; d: Integer): Boolean; overload; cdecl; external QtIntf name 'QDate_isValid2';
function QDate_isLeapYear(year: Integer): Boolean; cdecl; external QtIntf name 'QDate_isLeapYear';
function QDate_gregorianToJulian(y: Integer; m: Integer; d: Integer): LongWord; cdecl; external QtIntf name 'QDate_gregorianToJulian';
procedure QDate_julianToGregorian(jd: LongWord; y: PInteger; m: PInteger; d: PInteger); cdecl; external QtIntf name 'QDate_julianToGregorian';
procedure QDate_fromJulianDay(retval: QDateH; jd: Integer); cdecl; external QtIntf name 'QDate_fromJulianDay';
function QDate_toJulianDay(handle: QDateH): Integer; cdecl; external QtIntf name 'QDate_toJulianDay';

function QTime_create(): QTimeH; overload; cdecl; external QtIntf name 'QTime_create';
procedure QTime_destroy(handle: QTimeH); cdecl; external QtIntf name 'QTime_destroy'; 
function QTime_create(h: Integer; m: Integer; s: Integer = 0; ms: Integer = 0): QTimeH; overload; cdecl; external QtIntf name 'QTime_create2';
function QTime_isNull(handle: QTimeH): Boolean; cdecl; external QtIntf name 'QTime_isNull';
function QTime_isValid(handle: QTimeH): Boolean; overload; cdecl; external QtIntf name 'QTime_isValid';
function QTime_hour(handle: QTimeH): Integer; cdecl; external QtIntf name 'QTime_hour';
function QTime_minute(handle: QTimeH): Integer; cdecl; external QtIntf name 'QTime_minute';
function QTime_second(handle: QTimeH): Integer; cdecl; external QtIntf name 'QTime_second';
function QTime_msec(handle: QTimeH): Integer; cdecl; external QtIntf name 'QTime_msec';
procedure QTime_toString(handle: QTimeH; retval: PWideString; f: QtDateFormat = QtTextDate); overload; cdecl; external QtIntf name 'QTime_toString';
procedure QTime_toString(handle: QTimeH; retval: PWideString; format: PWideString); overload; cdecl; external QtIntf name 'QTime_toString2';
function QTime_setHMS(handle: QTimeH; h: Integer; m: Integer; s: Integer; ms: Integer = 0): Boolean; cdecl; external QtIntf name 'QTime_setHMS';
procedure QTime_addSecs(handle: QTimeH; retval: QTimeH; secs: Integer); cdecl; external QtIntf name 'QTime_addSecs';
function QTime_secsTo(handle: QTimeH; p1: QTimeH): Integer; cdecl; external QtIntf name 'QTime_secsTo';
procedure QTime_addMSecs(handle: QTimeH; retval: QTimeH; ms: Integer); cdecl; external QtIntf name 'QTime_addMSecs';
function QTime_msecsTo(handle: QTimeH; p1: QTimeH): Integer; cdecl; external QtIntf name 'QTime_msecsTo';
procedure QTime_currentTime(retval: QTimeH); cdecl; external QtIntf name 'QTime_currentTime';
procedure QTime_fromString(retval: QTimeH; s: PWideString; f: QtDateFormat = QtTextDate); overload; cdecl; external QtIntf name 'QTime_fromString';
procedure QTime_fromString(retval: QTimeH; s: PWideString; format: PWideString); overload; cdecl; external QtIntf name 'QTime_fromString2';
function QTime_isValid(h: Integer; m: Integer; s: Integer; ms: Integer = 0): Boolean; overload; cdecl; external QtIntf name 'QTime_isValid2';
procedure QTime_start(handle: QTimeH); cdecl; external QtIntf name 'QTime_start';
function QTime_restart(handle: QTimeH): Integer; cdecl; external QtIntf name 'QTime_restart';
function QTime_elapsed(handle: QTimeH): Integer; cdecl; external QtIntf name 'QTime_elapsed';

function QDateTime_create(): QDateTimeH; overload; cdecl; external QtIntf name 'QDateTime_create';
procedure QDateTime_destroy(handle: QDateTimeH); cdecl; external QtIntf name 'QDateTime_destroy'; 
function QDateTime_create(p1: QDateH): QDateTimeH; overload; cdecl; external QtIntf name 'QDateTime_create2';
function QDateTime_create(p1: QDateH; p2: QTimeH; spec: QtTimeSpec = QtLocalTime): QDateTimeH; overload; cdecl; external QtIntf name 'QDateTime_create3';
function QDateTime_create(other: QDateTimeH): QDateTimeH; overload; cdecl; external QtIntf name 'QDateTime_create4';
function QDateTime_isNull(handle: QDateTimeH): Boolean; cdecl; external QtIntf name 'QDateTime_isNull';
function QDateTime_isValid(handle: QDateTimeH): Boolean; cdecl; external QtIntf name 'QDateTime_isValid';
procedure QDateTime_date(handle: QDateTimeH; retval: QDateH); cdecl; external QtIntf name 'QDateTime_date';
procedure QDateTime_time(handle: QDateTimeH; retval: QTimeH); cdecl; external QtIntf name 'QDateTime_time';
function QDateTime_timeSpec(handle: QDateTimeH): QtTimeSpec; cdecl; external QtIntf name 'QDateTime_timeSpec';
function QDateTime_toTime_t(handle: QDateTimeH): LongWord; cdecl; external QtIntf name 'QDateTime_toTime_t';
procedure QDateTime_setDate(handle: QDateTimeH; date: QDateH); cdecl; external QtIntf name 'QDateTime_setDate';
procedure QDateTime_setTime(handle: QDateTimeH; time: QTimeH); cdecl; external QtIntf name 'QDateTime_setTime';
procedure QDateTime_setTimeSpec(handle: QDateTimeH; spec: QtTimeSpec); cdecl; external QtIntf name 'QDateTime_setTimeSpec';
procedure QDateTime_setTime_t(handle: QDateTimeH; secsSince1Jan1970UTC: LongWord); cdecl; external QtIntf name 'QDateTime_setTime_t';
procedure QDateTime_toString(handle: QDateTimeH; retval: PWideString; f: QtDateFormat = QtTextDate); overload; cdecl; external QtIntf name 'QDateTime_toString';
procedure QDateTime_toString(handle: QDateTimeH; retval: PWideString; format: PWideString); overload; cdecl; external QtIntf name 'QDateTime_toString2';
procedure QDateTime_addDays(handle: QDateTimeH; retval: QDateTimeH; days: Integer); cdecl; external QtIntf name 'QDateTime_addDays';
procedure QDateTime_addMonths(handle: QDateTimeH; retval: QDateTimeH; months: Integer); cdecl; external QtIntf name 'QDateTime_addMonths';
procedure QDateTime_addYears(handle: QDateTimeH; retval: QDateTimeH; years: Integer); cdecl; external QtIntf name 'QDateTime_addYears';
procedure QDateTime_addSecs(handle: QDateTimeH; retval: QDateTimeH; secs: Integer); cdecl; external QtIntf name 'QDateTime_addSecs';
procedure QDateTime_addMSecs(handle: QDateTimeH; retval: QDateTimeH; msecs: int64); cdecl; external QtIntf name 'QDateTime_addMSecs';
procedure QDateTime_toTimeSpec(handle: QDateTimeH; retval: QDateTimeH; spec: QtTimeSpec); cdecl; external QtIntf name 'QDateTime_toTimeSpec';
procedure QDateTime_toLocalTime(handle: QDateTimeH; retval: QDateTimeH); cdecl; external QtIntf name 'QDateTime_toLocalTime';
procedure QDateTime_toUTC(handle: QDateTimeH; retval: QDateTimeH); cdecl; external QtIntf name 'QDateTime_toUTC';
function QDateTime_daysTo(handle: QDateTimeH; p1: QDateTimeH): Integer; cdecl; external QtIntf name 'QDateTime_daysTo';
function QDateTime_secsTo(handle: QDateTimeH; p1: QDateTimeH): Integer; cdecl; external QtIntf name 'QDateTime_secsTo';
procedure QDateTime_currentDateTime(retval: QDateTimeH); cdecl; external QtIntf name 'QDateTime_currentDateTime';
procedure QDateTime_fromString(retval: QDateTimeH; s: PWideString; f: QtDateFormat = QtTextDate); overload; cdecl; external QtIntf name 'QDateTime_fromString';
procedure QDateTime_fromString(retval: QDateTimeH; s: PWideString; format: PWideString); overload; cdecl; external QtIntf name 'QDateTime_fromString2';
procedure QDateTime_fromTime_t(retval: QDateTimeH; secsSince1Jan1970UTC: LongWord); cdecl; external QtIntf name 'QDateTime_fromTime_t';

function QByteArray_create(): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_create';
procedure QByteArray_destroy(handle: QByteArrayH); cdecl; external QtIntf name 'QByteArray_destroy'; 
function QByteArray_create(p1: PAnsiChar): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_create2';
function QByteArray_create(p1: PAnsiChar; size: Integer): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_create3';
function QByteArray_create(size: Integer; c: char): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_create4';
function QByteArray_create(p1: QByteArrayH): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_create5';
function QByteArray_size(handle: QByteArrayH): Integer; cdecl; external QtIntf name 'QByteArray_size';
function QByteArray_isEmpty(handle: QByteArrayH): Boolean; cdecl; external QtIntf name 'QByteArray_isEmpty';
procedure QByteArray_resize(handle: QByteArrayH; size: Integer); cdecl; external QtIntf name 'QByteArray_resize';
function QByteArray_fill(handle: QByteArrayH; c: char; size: Integer = -1): QByteArrayH; cdecl; external QtIntf name 'QByteArray_fill';
function QByteArray_capacity(handle: QByteArrayH): Integer; cdecl; external QtIntf name 'QByteArray_capacity';
procedure QByteArray_reserve(handle: QByteArrayH; size: Integer); cdecl; external QtIntf name 'QByteArray_reserve';
procedure QByteArray_squeeze(handle: QByteArrayH); cdecl; external QtIntf name 'QByteArray_squeeze';
function QByteArray_data(handle: QByteArrayH): PAnsiChar; overload; cdecl; external QtIntf name 'QByteArray_data';
function QByteArray_constData(handle: QByteArrayH): PAnsiChar; cdecl; external QtIntf name 'QByteArray_constData';
procedure QByteArray_detach(handle: QByteArrayH); cdecl; external QtIntf name 'QByteArray_detach';
function QByteArray_isDetached(handle: QByteArrayH): Boolean; cdecl; external QtIntf name 'QByteArray_isDetached';
procedure QByteArray_clear(handle: QByteArrayH); cdecl; external QtIntf name 'QByteArray_clear';
function QByteArray_at(handle: QByteArrayH; i: Integer): char; cdecl; external QtIntf name 'QByteArray_at';
function QByteArray_indexOf(handle: QByteArrayH; c: char; from: Integer = 0): Integer; overload; cdecl; external QtIntf name 'QByteArray_indexOf';
function QByteArray_indexOf(handle: QByteArrayH; c: PAnsiChar; from: Integer = 0): Integer; overload; cdecl; external QtIntf name 'QByteArray_indexOf2';
function QByteArray_indexOf(handle: QByteArrayH; a: QByteArrayH; from: Integer = 0): Integer; overload; cdecl; external QtIntf name 'QByteArray_indexOf3';
function QByteArray_lastIndexOf(handle: QByteArrayH; c: char; from: Integer = -1): Integer; overload; cdecl; external QtIntf name 'QByteArray_lastIndexOf';
function QByteArray_lastIndexOf(handle: QByteArrayH; c: PAnsiChar; from: Integer = -1): Integer; overload; cdecl; external QtIntf name 'QByteArray_lastIndexOf2';
function QByteArray_lastIndexOf(handle: QByteArrayH; a: QByteArrayH; from: Integer = -1): Integer; overload; cdecl; external QtIntf name 'QByteArray_lastIndexOf3';
function QByteArray_contains(handle: QByteArrayH; c: char): boolean; overload; cdecl; external QtIntf name 'QByteArray_contains';
function QByteArray_contains(handle: QByteArrayH; a: PAnsiChar): boolean; overload; cdecl; external QtIntf name 'QByteArray_contains2';
function QByteArray_contains(handle: QByteArrayH; a: QByteArrayH): boolean; overload; cdecl; external QtIntf name 'QByteArray_contains3';
function QByteArray_count(handle: QByteArrayH; c: char): Integer; overload; cdecl; external QtIntf name 'QByteArray_count';
function QByteArray_count(handle: QByteArrayH; a: PAnsiChar): Integer; overload; cdecl; external QtIntf name 'QByteArray_count2';
function QByteArray_count(handle: QByteArrayH; a: QByteArrayH): Integer; overload; cdecl; external QtIntf name 'QByteArray_count3';
procedure QByteArray_left(handle: QByteArrayH; retval: QByteArrayH; len: Integer); cdecl; external QtIntf name 'QByteArray_left';
procedure QByteArray_right(handle: QByteArrayH; retval: QByteArrayH; len: Integer); cdecl; external QtIntf name 'QByteArray_right';
procedure QByteArray_mid(handle: QByteArrayH; retval: QByteArrayH; index: Integer; len: Integer = -1); cdecl; external QtIntf name 'QByteArray_mid';
function QByteArray_startsWith(handle: QByteArrayH; a: QByteArrayH): Boolean; overload; cdecl; external QtIntf name 'QByteArray_startsWith';
function QByteArray_startsWith(handle: QByteArrayH; c: char): Boolean; overload; cdecl; external QtIntf name 'QByteArray_startsWith2';
function QByteArray_startsWith(handle: QByteArrayH; c: PAnsiChar): Boolean; overload; cdecl; external QtIntf name 'QByteArray_startsWith3';
function QByteArray_endsWith(handle: QByteArrayH; a: QByteArrayH): Boolean; overload; cdecl; external QtIntf name 'QByteArray_endsWith';
function QByteArray_endsWith(handle: QByteArrayH; c: char): Boolean; overload; cdecl; external QtIntf name 'QByteArray_endsWith2';
function QByteArray_endsWith(handle: QByteArrayH; c: PAnsiChar): Boolean; overload; cdecl; external QtIntf name 'QByteArray_endsWith3';
procedure QByteArray_truncate(handle: QByteArrayH; pos: Integer); cdecl; external QtIntf name 'QByteArray_truncate';
procedure QByteArray_chop(handle: QByteArrayH; n: Integer); cdecl; external QtIntf name 'QByteArray_chop';
procedure QByteArray_toLower(handle: QByteArrayH; retval: QByteArrayH); cdecl; external QtIntf name 'QByteArray_toLower';
procedure QByteArray_toUpper(handle: QByteArrayH; retval: QByteArrayH); cdecl; external QtIntf name 'QByteArray_toUpper';
procedure QByteArray_trimmed(handle: QByteArrayH; retval: QByteArrayH); cdecl; external QtIntf name 'QByteArray_trimmed';
procedure QByteArray_simplified(handle: QByteArrayH; retval: QByteArrayH); cdecl; external QtIntf name 'QByteArray_simplified';
procedure QByteArray_leftJustified(handle: QByteArrayH; retval: QByteArrayH; width: Integer; fill: char; truncate: Boolean = False); cdecl; external QtIntf name 'QByteArray_leftJustified';
procedure QByteArray_rightJustified(handle: QByteArrayH; retval: QByteArrayH; width: Integer; fill: char; truncate: Boolean = False); cdecl; external QtIntf name 'QByteArray_rightJustified';
function QByteArray_prepend(handle: QByteArrayH; c: char): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_prepend';
function QByteArray_prepend(handle: QByteArrayH; s: PAnsiChar): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_prepend2';
function QByteArray_prepend(handle: QByteArrayH; a: QByteArrayH): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_prepend3';
function QByteArray_append(handle: QByteArrayH; c: char): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_append';
function QByteArray_append(handle: QByteArrayH; s: PAnsiChar): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_append2';
function QByteArray_append(handle: QByteArrayH; a: QByteArrayH): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_append3';
function QByteArray_insert(handle: QByteArrayH; i: Integer; c: char): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_insert';
function QByteArray_insert(handle: QByteArrayH; i: Integer; s: PAnsiChar): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_insert2';
function QByteArray_insert(handle: QByteArrayH; i: Integer; a: QByteArrayH): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_insert3';
function QByteArray_remove(handle: QByteArrayH; index: Integer; len: Integer): QByteArrayH; cdecl; external QtIntf name 'QByteArray_remove';
function QByteArray_replace(handle: QByteArrayH; index: Integer; len: Integer; s: PAnsiChar): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace';
function QByteArray_replace(handle: QByteArrayH; index: Integer; len: Integer; s: QByteArrayH): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace2';
function QByteArray_replace(handle: QByteArrayH; before: char; after: PAnsiChar): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace3';
function QByteArray_replace(handle: QByteArrayH; before: char; after: QByteArrayH): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace4';
function QByteArray_replace(handle: QByteArrayH; before: PAnsiChar; after: PAnsiChar): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace5';
function QByteArray_replace(handle: QByteArrayH; before: QByteArrayH; after: QByteArrayH): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace6';
function QByteArray_replace(handle: QByteArrayH; before: QByteArrayH; after: PAnsiChar): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace7';
function QByteArray_replace(handle: QByteArrayH; before: PAnsiChar; after: QByteArrayH): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace8';
function QByteArray_replace(handle: QByteArrayH; before: char; after: char): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace9';
function QByteArray_append(handle: QByteArrayH; s: PWideString): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_append4';
function QByteArray_insert(handle: QByteArrayH; i: Integer; s: PWideString): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_insert4';
function QByteArray_replace(handle: QByteArrayH; before: PWideString; after: PAnsiChar): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace10';
function QByteArray_replace(handle: QByteArrayH; c: char; after: PWideString): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace11';
function QByteArray_replace(handle: QByteArrayH; before: PWideString; after: QByteArrayH): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_replace12';
function QByteArray_indexOf(handle: QByteArrayH; s: PWideString; from: Integer = 0): Integer; overload; cdecl; external QtIntf name 'QByteArray_indexOf4';
function QByteArray_lastIndexOf(handle: QByteArrayH; s: PWideString; from: Integer = -1): Integer; overload; cdecl; external QtIntf name 'QByteArray_lastIndexOf4';
function QByteArray_toShort(handle: QByteArrayH; ok: PBoolean = nil; base: Integer = 10): ShortInt; cdecl; external QtIntf name 'QByteArray_toShort';
function QByteArray_toUShort(handle: QByteArrayH; ok: PBoolean = nil; base: Integer = 10): Word; cdecl; external QtIntf name 'QByteArray_toUShort';
function QByteArray_toInt(handle: QByteArrayH; ok: PBoolean = nil; base: Integer = 10): Integer; cdecl; external QtIntf name 'QByteArray_toInt';
function QByteArray_toUInt(handle: QByteArrayH; ok: PBoolean = nil; base: Integer = 10): LongWord; cdecl; external QtIntf name 'QByteArray_toUInt';
function QByteArray_toLong(handle: QByteArrayH; ok: PBoolean = nil; base: Integer = 10): Long; cdecl; external QtIntf name 'QByteArray_toLong';
function QByteArray_toULong(handle: QByteArrayH; ok: PBoolean = nil; base: Integer = 10): Longword; cdecl; external QtIntf name 'QByteArray_toULong';
function QByteArray_toLongLong(handle: QByteArrayH; ok: PBoolean = nil; base: Integer = 10): int64; cdecl; external QtIntf name 'QByteArray_toLongLong';
function QByteArray_toULongLong(handle: QByteArrayH; ok: PBoolean = nil; base: Integer = 10): qword; cdecl; external QtIntf name 'QByteArray_toULongLong';
function QByteArray_toFloat(handle: QByteArrayH; ok: PBoolean = nil): Single; cdecl; external QtIntf name 'QByteArray_toFloat';
function QByteArray_toDouble(handle: QByteArrayH; ok: PBoolean = nil): Double; cdecl; external QtIntf name 'QByteArray_toDouble';
procedure QByteArray_toBase64(handle: QByteArrayH; retval: QByteArrayH); cdecl; external QtIntf name 'QByteArray_toBase64';
function QByteArray_setNum(handle: QByteArrayH; p1: ShortInt; base: Integer = 10): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_setNum';
function QByteArray_setNum(handle: QByteArrayH; p1: Word; base: Integer = 10): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_setNum2';
function QByteArray_setNum(handle: QByteArrayH; p1: Integer; base: Integer = 10): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_setNum3';
function QByteArray_setNum(handle: QByteArrayH; p1: LongWord; base: Integer = 10): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_setNum4';
function QByteArray_setNum(handle: QByteArrayH; p1: int64; base: Integer = 10): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_setNum5';
function QByteArray_setNum(handle: QByteArrayH; p1: qword; base: Integer = 10): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_setNum6';
function QByteArray_setNum(handle: QByteArrayH; p1: Single; f: char; prec: Integer = 6): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_setNum7';
function QByteArray_setNum(handle: QByteArrayH; p1: Double; f: char; prec: Integer = 6): QByteArrayH; overload; cdecl; external QtIntf name 'QByteArray_setNum8';
procedure QByteArray_number(retval: QByteArrayH; p1: Integer; base: Integer = 10); overload; cdecl; external QtIntf name 'QByteArray_number';
procedure QByteArray_number(retval: QByteArrayH; p1: LongWord; base: Integer = 10); overload; cdecl; external QtIntf name 'QByteArray_number2';
procedure QByteArray_number(retval: QByteArrayH; p1: int64; base: Integer = 10); overload; cdecl; external QtIntf name 'QByteArray_number3';
procedure QByteArray_number(retval: QByteArrayH; p1: qword; base: Integer = 10); overload; cdecl; external QtIntf name 'QByteArray_number4';
procedure QByteArray_number(retval: QByteArrayH; p1: Double; f: char; prec: Integer = 6); overload; cdecl; external QtIntf name 'QByteArray_number5';
procedure QByteArray_fromRawData(retval: QByteArrayH; p1: PAnsiChar; size: Integer); cdecl; external QtIntf name 'QByteArray_fromRawData';
procedure QByteArray_fromBase64(retval: QByteArrayH; base64: QByteArrayH); cdecl; external QtIntf name 'QByteArray_fromBase64';
procedure QByteArray_push_back(handle: QByteArrayH; c: char); overload; cdecl; external QtIntf name 'QByteArray_push_back';
procedure QByteArray_push_back(handle: QByteArrayH; c: PAnsiChar); overload; cdecl; external QtIntf name 'QByteArray_push_back2';
procedure QByteArray_push_back(handle: QByteArrayH; a: QByteArrayH); overload; cdecl; external QtIntf name 'QByteArray_push_back3';
procedure QByteArray_push_front(handle: QByteArrayH; c: char); overload; cdecl; external QtIntf name 'QByteArray_push_front';
procedure QByteArray_push_front(handle: QByteArrayH; c: PAnsiChar); overload; cdecl; external QtIntf name 'QByteArray_push_front2';
procedure QByteArray_push_front(handle: QByteArrayH; a: QByteArrayH); overload; cdecl; external QtIntf name 'QByteArray_push_front3';
function QByteArray_count(handle: QByteArrayH): Integer; overload; cdecl; external QtIntf name 'QByteArray_count4';
function QByteArray_length(handle: QByteArrayH): Integer; cdecl; external QtIntf name 'QByteArray_length';
function QByteArray_isNull(handle: QByteArrayH): Boolean; cdecl; external QtIntf name 'QByteArray_isNull';



type
  QLocaleFormatType = ( // QLocale::FormatType (1)
    QLocaleLongFormat, QLocaleShortFormat );

type
  QLocaleNumberOption = cardinal; // QLocale::NumberOption
  QLocaleNumberOptions = QLocaleNumberOption; //QFlags<> (3)
const
  QLocaleOmitGroupSeparator =   $01;
  QLocaleRejectGroupSeparator =   $02;


type
  QSystemLocaleQueryType = ( // QSystemLocale::QueryType (1)
    QSystemLocaleLanguageId, QSystemLocaleCountryId, QSystemLocaleDecimalPoint, QSystemLocaleGroupSeparator, QSystemLocaleZeroDigit, QSystemLocaleNegativeSign, QSystemLocaleDateFormatLong, 
    QSystemLocaleDateFormatShort, QSystemLocaleTimeFormatLong, QSystemLocaleTimeFormatShort, QSystemLocaleDayNameLong, QSystemLocaleDayNameShort, QSystemLocaleMonthNameLong, QSystemLocaleMonthNameShort, 
    QSystemLocaleDateToStringLong, QSystemLocaleDateToStringShort, QSystemLocaleTimeToStringLong, QSystemLocaleTimeToStringShort );

type
  QLocaleLanguage = cardinal; //  QLocale::Language (4)

const
    QLocaleC = 1 { $1 };
    QLocaleAbkhazian = 2 { $2 };
    QLocaleAfan = 3 { $3 };
    QLocaleAfar = 4 { $4 };
    QLocaleAfrikaans = 5 { $5 };
    QLocaleAlbanian = 6 { $6 };
    QLocaleAmharic = 7 { $7 };
    QLocaleArabic = 8 { $8 };
    QLocaleArmenian = 9 { $9 };
    QLocaleAssamese = 10 { $a };
    QLocaleAymara = 11 { $b };
    QLocaleAzerbaijani = 12 { $c };
    QLocaleBashkir = 13 { $d };
    QLocaleBasque = 14 { $e };
    QLocaleBengali = 15 { $f };
    QLocaleBhutani = 16 { $10 };
    QLocaleBihari = 17 { $11 };
    QLocaleBislama = 18 { $12 };
    QLocaleBreton = 19 { $13 };
    QLocaleBulgarian = 20 { $14 };
    QLocaleBurmese = 21 { $15 };
    QLocaleByelorussian = 22 { $16 };
    QLocaleCambodian = 23 { $17 };
    QLocaleCatalan = 24 { $18 };
    QLocaleChinese = 25 { $19 };
    QLocaleCorsican = 26 { $1a };
    QLocaleCroatian = 27 { $1b };
    QLocaleCzech = 28 { $1c };
    QLocaleDanish = 29 { $1d };
    QLocaleDutch = 30 { $1e };
    QLocaleEnglish = 31 { $1f };
    QLocaleEsperanto = 32 { $20 };
    QLocaleEstonian = 33 { $21 };
    QLocaleFaroese = 34 { $22 };
    QLocaleFijiLanguage = 35 { $23 };
    QLocaleFinnish = 36 { $24 };
    QLocaleFrench = 37 { $25 };
    QLocaleFrisian = 38 { $26 };
    QLocaleGaelic = 39 { $27 };
    QLocaleGalician = 40 { $28 };
    QLocaleGeorgian = 41 { $29 };
    QLocaleGerman = 42 { $2a };
    QLocaleGreek = 43 { $2b };
    QLocaleGreenlandic = 44 { $2c };
    QLocaleGuarani = 45 { $2d };
    QLocaleGujarati = 46 { $2e };
    QLocaleHausa = 47 { $2f };
    QLocaleHebrew = 48 { $30 };
    QLocaleHindi = 49 { $31 };
    QLocaleHungarian = 50 { $32 };
    QLocaleIcelandic = 51 { $33 };
    QLocaleIndonesian = 52 { $34 };
    QLocaleInterlingua = 53 { $35 };
    QLocaleInterlingue = 54 { $36 };
    QLocaleInuktitut = 55 { $37 };
    QLocaleInupiak = 56 { $38 };
    QLocaleIrish = 57 { $39 };
    QLocaleItalian = 58 { $3a };
    QLocaleJapanese = 59 { $3b };
    QLocaleJavanese = 60 { $3c };
    QLocaleKannada = 61 { $3d };
    QLocaleKashmiri = 62 { $3e };
    QLocaleKazakh = 63 { $3f };
    QLocaleKinyarwanda = 64 { $40 };
    QLocaleKirghiz = 65 { $41 };
    QLocaleKorean = 66 { $42 };
    QLocaleKurdish = 67 { $43 };
    QLocaleKurundi = 68 { $44 };
    QLocaleLaothian = 69 { $45 };
    QLocaleLatin = 70 { $46 };
    QLocaleLatvian = 71 { $47 };
    QLocaleLingala = 72 { $48 };
    QLocaleLithuanian = 73 { $49 };
    QLocaleMacedonian = 74 { $4a };
    QLocaleMalagasy = 75 { $4b };
    QLocaleMalay = 76 { $4c };
    QLocaleMalayalam = 77 { $4d };
    QLocaleMaltese = 78 { $4e };
    QLocaleMaori = 79 { $4f };
    QLocaleMarathi = 80 { $50 };
    QLocaleMoldavian = 81 { $51 };
    QLocaleMongolian = 82 { $52 };
    QLocaleNauruLanguage = 83 { $53 };
    QLocaleNepali = 84 { $54 };
    QLocaleNorwegian = 85 { $55 };
    QLocaleOccitan = 86 { $56 };
    QLocaleOriya = 87 { $57 };
    QLocalePashto = 88 { $58 };
    QLocalePersian = 89 { $59 };
    QLocalePolish = 90 { $5a };
    QLocalePortuguese = 91 { $5b };
    QLocalePunjabi = 92 { $5c };
    QLocaleQuechua = 93 { $5d };
    QLocaleRhaetoRomance = 94 { $5e };
    QLocaleRomanian = 95 { $5f };
    QLocaleRussian = 96 { $60 };
    QLocaleSamoan = 97 { $61 };
    QLocaleSangho = 98 { $62 };
    QLocaleSanskrit = 99 { $63 };
    QLocaleSerbian = 100 { $64 };
    QLocaleSerboCroatian = 101 { $65 };
    QLocaleSesotho = 102 { $66 };
    QLocaleSetswana = 103 { $67 };
    QLocaleShona = 104 { $68 };
    QLocaleSindhi = 105 { $69 };
    QLocaleSinghalese = 106 { $6a };
    QLocaleSiswati = 107 { $6b };
    QLocaleSlovak = 108 { $6c };
    QLocaleSlovenian = 109 { $6d };
    QLocaleSomali = 110 { $6e };
    QLocaleSpanish = 111 { $6f };
    QLocaleSundanese = 112 { $70 };
    QLocaleSwahili = 113 { $71 };
    QLocaleSwedish = 114 { $72 };
    QLocaleTagalog = 115 { $73 };
    QLocaleTajik = 116 { $74 };
    QLocaleTamil = 117 { $75 };
    QLocaleTatar = 118 { $76 };
    QLocaleTelugu = 119 { $77 };
    QLocaleThai = 120 { $78 };
    QLocaleTibetan = 121 { $79 };
    QLocaleTigrinya = 122 { $7a };
    QLocaleTongaLanguage = 123 { $7b };
    QLocaleTsonga = 124 { $7c };
    QLocaleTurkish = 125 { $7d };
    QLocaleTurkmen = 126 { $7e };
    QLocaleTwi = 127 { $7f };
    QLocaleUigur = 128 { $80 };
    QLocaleUkrainian = 129 { $81 };
    QLocaleUrdu = 130 { $82 };
    QLocaleUzbek = 131 { $83 };
    QLocaleVietnamese = 132 { $84 };
    QLocaleVolapuk = 133 { $85 };
    QLocaleWelsh = 134 { $86 };
    QLocaleWolof = 135 { $87 };
    QLocaleXhosa = 136 { $88 };
    QLocaleYiddish = 137 { $89 };
    QLocaleYoruba = 138 { $8a };
    QLocaleZhuang = 139 { $8b };
    QLocaleZulu = 140 { $8c };
    QLocaleNynorsk = 141 { $8d };
    QLocaleBosnian = 142 { $8e };
    QLocaleDivehi = 143 { $8f };
    QLocaleManx = 144 { $90 };
    QLocaleCornish = 145 { $91 };
    QLocaleLastLanguage = 145 { $91 };

type
  QLocaleCountry = cardinal; //  QLocale::Country (4)

const
    QLocaleAnyCountry = 0 { $0 };
    QLocaleAfghanistan = 1 { $1 };
    QLocaleAlbania = 2 { $2 };
    QLocaleAlgeria = 3 { $3 };
    QLocaleAmericanSamoa = 4 { $4 };
    QLocaleAndorra = 5 { $5 };
    QLocaleAngola = 6 { $6 };
    QLocaleAnguilla = 7 { $7 };
    QLocaleAntarctica = 8 { $8 };
    QLocaleAntiguaAndBarbuda = 9 { $9 };
    QLocaleArgentina = 10 { $a };
    QLocaleArmenia = 11 { $b };
    QLocaleAruba = 12 { $c };
    QLocaleAustralia = 13 { $d };
    QLocaleAustria = 14 { $e };
    QLocaleAzerbaijan = 15 { $f };
    QLocaleBahamas = 16 { $10 };
    QLocaleBahrain = 17 { $11 };
    QLocaleBangladesh = 18 { $12 };
    QLocaleBarbados = 19 { $13 };
    QLocaleBelarus = 20 { $14 };
    QLocaleBelgium = 21 { $15 };
    QLocaleBelize = 22 { $16 };
    QLocaleBenin = 23 { $17 };
    QLocaleBermuda = 24 { $18 };
    QLocaleBhutan = 25 { $19 };
    QLocaleBolivia = 26 { $1a };
    QLocaleBosniaAndHerzegowina = 27 { $1b };
    QLocaleBotswana = 28 { $1c };
    QLocaleBouvetIsland = 29 { $1d };
    QLocaleBrazil = 30 { $1e };
    QLocaleBritishIndianOceanTerritory = 31 { $1f };
    QLocaleBruneiDarussalam = 32 { $20 };
    QLocaleBulgaria = 33 { $21 };
    QLocaleBurkinaFaso = 34 { $22 };
    QLocaleBurundi = 35 { $23 };
    QLocaleCambodia = 36 { $24 };
    QLocaleCameroon = 37 { $25 };
    QLocaleCanada = 38 { $26 };
    QLocaleCapeVerde = 39 { $27 };
    QLocaleCaymanIslands = 40 { $28 };
    QLocaleCentralAfricanRepublic = 41 { $29 };
    QLocaleChad = 42 { $2a };
    QLocaleChile = 43 { $2b };
    QLocaleChina = 44 { $2c };
    QLocaleChristmasIsland = 45 { $2d };
    QLocaleCocosIslands = 46 { $2e };
    QLocaleColombia = 47 { $2f };
    QLocaleComoros = 48 { $30 };
    QLocaleDemocraticRepublicOfCongo = 49 { $31 };
    QLocalePeoplesRepublicOfCongo = 50 { $32 };
    QLocaleCookIslands = 51 { $33 };
    QLocaleCostaRica = 52 { $34 };
    QLocaleIvoryCoast = 53 { $35 };
    QLocaleCroatia = 54 { $36 };
    QLocaleCuba = 55 { $37 };
    QLocaleCyprus = 56 { $38 };
    QLocaleCzechRepublic = 57 { $39 };
    QLocaleDenmark = 58 { $3a };
    QLocaleDjibouti = 59 { $3b };
    QLocaleDominica = 60 { $3c };
    QLocaleDominicanRepublic = 61 { $3d };
    QLocaleEastTimor = 62 { $3e };
    QLocaleEcuador = 63 { $3f };
    QLocaleEgypt = 64 { $40 };
    QLocaleElSalvador = 65 { $41 };
    QLocaleEquatorialGuinea = 66 { $42 };
    QLocaleEritrea = 67 { $43 };
    QLocaleEstonia = 68 { $44 };
    QLocaleEthiopia = 69 { $45 };
    QLocaleFalklandIslands = 70 { $46 };
    QLocaleFaroeIslands = 71 { $47 };
    QLocaleFijiCountry = 72 { $48 };
    QLocaleFinland = 73 { $49 };
    QLocaleFrance = 74 { $4a };
    QLocaleMetropolitanFrance = 75 { $4b };
    QLocaleFrenchGuiana = 76 { $4c };
    QLocaleFrenchPolynesia = 77 { $4d };
    QLocaleFrenchSouthernTerritories = 78 { $4e };
    QLocaleGabon = 79 { $4f };
    QLocaleGambia = 80 { $50 };
    QLocaleGeorgia = 81 { $51 };
    QLocaleGermany = 82 { $52 };
    QLocaleGhana = 83 { $53 };
    QLocaleGibraltar = 84 { $54 };
    QLocaleGreece = 85 { $55 };
    QLocaleGreenland = 86 { $56 };
    QLocaleGrenada = 87 { $57 };
    QLocaleGuadeloupe = 88 { $58 };
    QLocaleGuam = 89 { $59 };
    QLocaleGuatemala = 90 { $5a };
    QLocaleGuinea = 91 { $5b };
    QLocaleGuineaBissau = 92 { $5c };
    QLocaleGuyana = 93 { $5d };
    QLocaleHaiti = 94 { $5e };
    QLocaleHeardAndMcDonaldIslands = 95 { $5f };
    QLocaleHonduras = 96 { $60 };
    QLocaleHongKong = 97 { $61 };
    QLocaleHungary = 98 { $62 };
    QLocaleIceland = 99 { $63 };
    QLocaleIndia = 100 { $64 };
    QLocaleIndonesia = 101 { $65 };
    QLocaleIran = 102 { $66 };
    QLocaleIraq = 103 { $67 };
    QLocaleIreland = 104 { $68 };
    QLocaleIsrael = 105 { $69 };
    QLocaleItaly = 106 { $6a };
    QLocaleJamaica = 107 { $6b };
    QLocaleJapan = 108 { $6c };
    QLocaleJordan = 109 { $6d };
    QLocaleKazakhstan = 110 { $6e };
    QLocaleKenya = 111 { $6f };
    QLocaleKiribati = 112 { $70 };
    QLocaleDemocraticRepublicOfKorea = 113 { $71 };
    QLocaleRepublicOfKorea = 114 { $72 };
    QLocaleKuwait = 115 { $73 };
    QLocaleKyrgyzstan = 116 { $74 };
    QLocaleLao = 117 { $75 };
    QLocaleLatvia = 118 { $76 };
    QLocaleLebanon = 119 { $77 };
    QLocaleLesotho = 120 { $78 };
    QLocaleLiberia = 121 { $79 };
    QLocaleLibyanArabJamahiriya = 122 { $7a };
    QLocaleLiechtenstein = 123 { $7b };
    QLocaleLithuania = 124 { $7c };
    QLocaleLuxembourg = 125 { $7d };
    QLocaleMacau = 126 { $7e };
    QLocaleMacedonia = 127 { $7f };
    QLocaleMadagascar = 128 { $80 };
    QLocaleMalawi = 129 { $81 };
    QLocaleMalaysia = 130 { $82 };
    QLocaleMaldives = 131 { $83 };
    QLocaleMali = 132 { $84 };
    QLocaleMalta = 133 { $85 };
    QLocaleMarshallIslands = 134 { $86 };
    QLocaleMartinique = 135 { $87 };
    QLocaleMauritania = 136 { $88 };
    QLocaleMauritius = 137 { $89 };
    QLocaleMayotte = 138 { $8a };
    QLocaleMexico = 139 { $8b };
    QLocaleMicronesia = 140 { $8c };
    QLocaleMoldova = 141 { $8d };
    QLocaleMonaco = 142 { $8e };
    QLocaleMongolia = 143 { $8f };
    QLocaleMontserrat = 144 { $90 };
    QLocaleMorocco = 145 { $91 };
    QLocaleMozambique = 146 { $92 };
    QLocaleMyanmar = 147 { $93 };
    QLocaleNamibia = 148 { $94 };
    QLocaleNauruCountry = 149 { $95 };
    QLocaleNepal = 150 { $96 };
    QLocaleNetherlands = 151 { $97 };
    QLocaleNetherlandsAntilles = 152 { $98 };
    QLocaleNewCaledonia = 153 { $99 };
    QLocaleNewZealand = 154 { $9a };
    QLocaleNicaragua = 155 { $9b };
    QLocaleNiger = 156 { $9c };
    QLocaleNigeria = 157 { $9d };
    QLocaleNiue = 158 { $9e };
    QLocaleNorfolkIsland = 159 { $9f };
    QLocaleNorthernMarianaIslands = 160 { $a0 };
    QLocaleNorway = 161 { $a1 };
    QLocaleOman = 162 { $a2 };
    QLocalePakistan = 163 { $a3 };
    QLocalePalau = 164 { $a4 };
    QLocalePalestinianTerritory = 165 { $a5 };
    QLocalePanama = 166 { $a6 };
    QLocalePapuaNewGuinea = 167 { $a7 };
    QLocaleParaguay = 168 { $a8 };
    QLocalePeru = 169 { $a9 };
    QLocalePhilippines = 170 { $aa };
    QLocalePitcairn = 171 { $ab };
    QLocalePoland = 172 { $ac };
    QLocalePortugal = 173 { $ad };
    QLocalePuertoRico = 174 { $ae };
    QLocaleQatar = 175 { $af };
    QLocaleReunion = 176 { $b0 };
    QLocaleRomania = 177 { $b1 };
    QLocaleRussianFederation = 178 { $b2 };
    QLocaleRwanda = 179 { $b3 };
    QLocaleSaintKittsAndNevis = 180 { $b4 };
    QLocaleStLucia = 181 { $b5 };
    QLocaleStVincentAndTheGrenadines = 182 { $b6 };
    QLocaleSamoa = 183 { $b7 };
    QLocaleSanMarino = 184 { $b8 };
    QLocaleSaoTomeAndPrincipe = 185 { $b9 };
    QLocaleSaudiArabia = 186 { $ba };
    QLocaleSenegal = 187 { $bb };
    QLocaleSeychelles = 188 { $bc };
    QLocaleSierraLeone = 189 { $bd };
    QLocaleSingapore = 190 { $be };
    QLocaleSlovakia = 191 { $bf };
    QLocaleSlovenia = 192 { $c0 };
    QLocaleSolomonIslands = 193 { $c1 };
    QLocaleSomalia = 194 { $c2 };
    QLocaleSouthAfrica = 195 { $c3 };
    QLocaleSouthGeorgiaAndTheSouthSandwichIslands = 196 { $c4 };
    QLocaleSpain = 197 { $c5 };
    QLocaleSriLanka = 198 { $c6 };
    QLocaleStHelena = 199 { $c7 };
    QLocaleStPierreAndMiquelon = 200 { $c8 };
    QLocaleSudan = 201 { $c9 };
    QLocaleSuriname = 202 { $ca };
    QLocaleSvalbardAndJanMayenIslands = 203 { $cb };
    QLocaleSwaziland = 204 { $cc };
    QLocaleSweden = 205 { $cd };
    QLocaleSwitzerland = 206 { $ce };
    QLocaleSyrianArabRepublic = 207 { $cf };
    QLocaleTaiwan = 208 { $d0 };
    QLocaleTajikistan = 209 { $d1 };
    QLocaleTanzania = 210 { $d2 };
    QLocaleThailand = 211 { $d3 };
    QLocaleTogo = 212 { $d4 };
    QLocaleTokelau = 213 { $d5 };
    QLocaleTongaCountry = 214 { $d6 };
    QLocaleTrinidadAndTobago = 215 { $d7 };
    QLocaleTunisia = 216 { $d8 };
    QLocaleTurkey = 217 { $d9 };
    QLocaleTurkmenistan = 218 { $da };
    QLocaleTurksAndCaicosIslands = 219 { $db };
    QLocaleTuvalu = 220 { $dc };
    QLocaleUganda = 221 { $dd };
    QLocaleUkraine = 222 { $de };
    QLocaleUnitedArabEmirates = 223 { $df };
    QLocaleUnitedKingdom = 224 { $e0 };
    QLocaleUnitedStates = 225 { $e1 };
    QLocaleUnitedStatesMinorOutlyingIslands = 226 { $e2 };
    QLocaleUruguay = 227 { $e3 };
    QLocaleUzbekistan = 228 { $e4 };
    QLocaleVanuatu = 229 { $e5 };
    QLocaleVaticanCityState = 230 { $e6 };
    QLocaleVenezuela = 231 { $e7 };
    QLocaleVietNam = 232 { $e8 };
    QLocaleBritishVirginIslands = 233 { $e9 };
    QLocaleUSVirginIslands = 234 { $ea };
    QLocaleWallisAndFutunaIslands = 235 { $eb };
    QLocaleWesternSahara = 236 { $ec };
    QLocaleYemen = 237 { $ed };
    QLocaleYugoslavia = 238 { $ee };
    QLocaleZambia = 239 { $ef };
    QLocaleZimbabwe = 240 { $f0 };
    QLocaleSerbiaAndMontenegro = 241 { $f1 };
    QLocaleLastCountry = 241 { $f1 };


function QLocale_create(): QLocaleH; overload; cdecl; external QtIntf name 'QLocale_create';
procedure QLocale_destroy(handle: QLocaleH); cdecl; external QtIntf name 'QLocale_destroy'; 
function QLocale_create(name: PWideString): QLocaleH; overload; cdecl; external QtIntf name 'QLocale_create2';
function QLocale_create(language: QLocaleLanguage; country: QLocaleCountry = QLocaleAnyCountry): QLocaleH; overload; cdecl; external QtIntf name 'QLocale_create3';
function QLocale_create(other: QLocaleH): QLocaleH; overload; cdecl; external QtIntf name 'QLocale_create4';
function QLocale_language(handle: QLocaleH): QLocaleLanguage; cdecl; external QtIntf name 'QLocale_language';
function QLocale_country(handle: QLocaleH): QLocaleCountry; cdecl; external QtIntf name 'QLocale_country';
procedure QLocale_name(handle: QLocaleH; retval: PWideString); cdecl; external QtIntf name 'QLocale_name';
function QLocale_toShort(handle: QLocaleH; s: PWideString; ok: PBoolean = nil; base: Integer = 0): ShortInt; cdecl; external QtIntf name 'QLocale_toShort';
function QLocale_toUShort(handle: QLocaleH; s: PWideString; ok: PBoolean = nil; base: Integer = 0): Word; cdecl; external QtIntf name 'QLocale_toUShort';
function QLocale_toInt(handle: QLocaleH; s: PWideString; ok: PBoolean = nil; base: Integer = 0): Integer; cdecl; external QtIntf name 'QLocale_toInt';
function QLocale_toUInt(handle: QLocaleH; s: PWideString; ok: PBoolean = nil; base: Integer = 0): LongWord; cdecl; external QtIntf name 'QLocale_toUInt';
function QLocale_toLongLong(handle: QLocaleH; s: PWideString; ok: PBoolean = nil; base: Integer = 0): int64; cdecl; external QtIntf name 'QLocale_toLongLong';
function QLocale_toULongLong(handle: QLocaleH; s: PWideString; ok: PBoolean = nil; base: Integer = 0): int64; cdecl; external QtIntf name 'QLocale_toULongLong';
function QLocale_toFloat(handle: QLocaleH; s: PWideString; ok: PBoolean = nil): Single; cdecl; external QtIntf name 'QLocale_toFloat';
function QLocale_toDouble(handle: QLocaleH; s: PWideString; ok: PBoolean = nil): Double; cdecl; external QtIntf name 'QLocale_toDouble';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; i: int64); overload; cdecl; external QtIntf name 'QLocale_toString';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; i: qword); overload; cdecl; external QtIntf name 'QLocale_toString2';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; i: ShortInt); overload; cdecl; external QtIntf name 'QLocale_toString3';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; i: Word); overload; cdecl; external QtIntf name 'QLocale_toString4';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; i: Integer); overload; cdecl; external QtIntf name 'QLocale_toString5';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; i: LongWord); overload; cdecl; external QtIntf name 'QLocale_toString6';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; i: Double; f: char; prec: Integer = 6); overload; cdecl; external QtIntf name 'QLocale_toString7';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; i: Single; f: char; prec: Integer = 6); overload; cdecl; external QtIntf name 'QLocale_toString8';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; date: QDateH; formatStr: PWideString); overload; cdecl; external QtIntf name 'QLocale_toString9';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; date: QDateH; format: QLocaleFormatType = QLocaleLongFormat); overload; cdecl; external QtIntf name 'QLocale_toString10';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; time: QTimeH; formatStr: PWideString); overload; cdecl; external QtIntf name 'QLocale_toString11';
procedure QLocale_toString(handle: QLocaleH; retval: PWideString; time: QTimeH; format: QLocaleFormatType = QLocaleLongFormat); overload; cdecl; external QtIntf name 'QLocale_toString12';
procedure QLocale_dateFormat(handle: QLocaleH; retval: PWideString; format: QLocaleFormatType = QLocaleLongFormat); cdecl; external QtIntf name 'QLocale_dateFormat';
procedure QLocale_timeFormat(handle: QLocaleH; retval: PWideString; format: QLocaleFormatType = QLocaleLongFormat); cdecl; external QtIntf name 'QLocale_timeFormat';
procedure QLocale_decimalPoint(handle: QLocaleH; retval: PWideChar); cdecl; external QtIntf name 'QLocale_decimalPoint';
procedure QLocale_groupSeparator(handle: QLocaleH; retval: PWideChar); cdecl; external QtIntf name 'QLocale_groupSeparator';
procedure QLocale_percent(handle: QLocaleH; retval: PWideChar); cdecl; external QtIntf name 'QLocale_percent';
procedure QLocale_zeroDigit(handle: QLocaleH; retval: PWideChar); cdecl; external QtIntf name 'QLocale_zeroDigit';
procedure QLocale_negativeSign(handle: QLocaleH; retval: PWideChar); cdecl; external QtIntf name 'QLocale_negativeSign';
procedure QLocale_exponential(handle: QLocaleH; retval: PWideChar); cdecl; external QtIntf name 'QLocale_exponential';
procedure QLocale_monthName(handle: QLocaleH; retval: PWideString; p1: Integer; format: QLocaleFormatType = QLocaleLongFormat); cdecl; external QtIntf name 'QLocale_monthName';
procedure QLocale_dayName(handle: QLocaleH; retval: PWideString; p1: Integer; format: QLocaleFormatType = QLocaleLongFormat); cdecl; external QtIntf name 'QLocale_dayName';
procedure QLocale_languageToString(retval: PWideString; language: QLocaleLanguage); cdecl; external QtIntf name 'QLocale_languageToString';
procedure QLocale_countryToString(retval: PWideString; country: QLocaleCountry); cdecl; external QtIntf name 'QLocale_countryToString';
procedure QLocale_setDefault(locale: QLocaleH); cdecl; external QtIntf name 'QLocale_setDefault';
procedure QLocale_c(retval: QLocaleH); cdecl; external QtIntf name 'QLocale_c';
procedure QLocale_system(retval: QLocaleH); cdecl; external QtIntf name 'QLocale_system';
procedure QLocale_setNumberOptions(handle: QLocaleH; options: QLocaleNumberOptions); cdecl; external QtIntf name 'QLocale_setNumberOptions';
function QLocale_numberOptions(handle: QLocaleH): QLocaleNumberOptions; cdecl; external QtIntf name 'QLocale_numberOptions';

function QSystemLocale_create(): QSystemLocaleH; cdecl; external QtIntf name 'QSystemLocale_create';
procedure QSystemLocale_destroy(handle: QSystemLocaleH); cdecl; external QtIntf name 'QSystemLocale_destroy'; 
procedure QSystemLocale_fallbackLocale(handle: QSystemLocaleH; retval: QLocaleH); cdecl; external QtIntf name 'QSystemLocale_fallbackLocale';


type
  QApplicationType = ( // QApplication::Type (1)
    QApplicationTty, QApplicationGuiClient, QApplicationGuiServer );

  QApplicationColorSpec = (  //QApplication::ColorSpec (2s)
    QApplicationNormalColor = 0,
    QApplicationCustomColor = 1,
    QApplicationManyColor = 2 );

function QApplication_create(argc: PInteger; argv: PPAnsiChar; p3: Integer = QT_VERSION): QApplicationH; overload; cdecl; external QtIntf name 'QApplication_create';
procedure QApplication_destroy(handle: QApplicationH); cdecl; external QtIntf name 'QApplication_destroy'; 
function QApplication_create(argc: PInteger; argv: PPAnsiChar; GUIenabled: Boolean; p4: Integer = QT_VERSION): QApplicationH; overload; cdecl; external QtIntf name 'QApplication_create2';
function QApplication_create(argc: PInteger; argv: PPAnsiChar; p3: QApplicationType; p4: Integer = QT_VERSION): QApplicationH; overload; cdecl; external QtIntf name 'QApplication_create3';
{$ifdef UNIX }
function QApplication_create(dpy: PDisplay; visual: QtHANDLE = 0; cmap: QtHANDLE = 0; p4: Integer = QT_VERSION): QApplicationH; overload; cdecl; external QtIntf name 'QApplication_create4';
function QApplication_create(dpy: PDisplay; argc: PInteger; argv: PPAnsiChar; visual: QtHANDLE = 0; cmap: QtHANDLE = 0; p6: Integer = QT_VERSION): QApplicationH; overload; cdecl; external QtIntf name 'QApplication_create5';
{$endif}
function QApplication_type(): QApplicationType; cdecl; external QtIntf name 'QApplication_type';
function QApplication_style(): QStyleH; cdecl; external QtIntf name 'QApplication_style';
procedure QApplication_setStyle(p1: QStyleH); overload; cdecl; external QtIntf name 'QApplication_setStyle';
function QApplication_setStyle(p1: PWideString): QStyleH; overload; cdecl; external QtIntf name 'QApplication_setStyle2';
function QApplication_colorSpec(): Integer; cdecl; external QtIntf name 'QApplication_colorSpec';
procedure QApplication_setColorSpec(p1: Integer); cdecl; external QtIntf name 'QApplication_setColorSpec';
function QApplication_overrideCursor(): QCursorH; cdecl; external QtIntf name 'QApplication_overrideCursor';
procedure QApplication_setOverrideCursor(p1: QCursorH); cdecl; external QtIntf name 'QApplication_setOverrideCursor';
procedure QApplication_changeOverrideCursor(p1: QCursorH); cdecl; external QtIntf name 'QApplication_changeOverrideCursor';
procedure QApplication_restoreOverrideCursor(); cdecl; external QtIntf name 'QApplication_restoreOverrideCursor';
procedure QApplication_palette(retval: QPaletteH); overload; cdecl; external QtIntf name 'QApplication_palette';
procedure QApplication_palette(retval: QPaletteH; p1: QWidgetH); overload; cdecl; external QtIntf name 'QApplication_palette2';
procedure QApplication_palette(retval: QPaletteH; className: PAnsiChar); overload; cdecl; external QtIntf name 'QApplication_palette3';
procedure QApplication_setPalette(p1: QPaletteH; className: PAnsiChar = nil); cdecl; external QtIntf name 'QApplication_setPalette';
procedure QApplication_font(retval: QFontH); overload; cdecl; external QtIntf name 'QApplication_font';
procedure QApplication_font(retval: QFontH; p1: QWidgetH); overload; cdecl; external QtIntf name 'QApplication_font2';
procedure QApplication_font(retval: QFontH; className: PAnsiChar); overload; cdecl; external QtIntf name 'QApplication_font3';
procedure QApplication_setFont(p1: QFontH; className: PAnsiChar = nil); cdecl; external QtIntf name 'QApplication_setFont';
procedure QApplication_fontMetrics(retval: QFontMetricsH); cdecl; external QtIntf name 'QApplication_fontMetrics';
procedure QApplication_setWindowIcon(icon: QIconH); cdecl; external QtIntf name 'QApplication_setWindowIcon';
procedure QApplication_windowIcon(retval: QIconH); cdecl; external QtIntf name 'QApplication_windowIcon';
function QApplication_desktop(): QDesktopWidgetH; cdecl; external QtIntf name 'QApplication_desktop';
function QApplication_activePopupWidget(): QWidgetH; cdecl; external QtIntf name 'QApplication_activePopupWidget';
function QApplication_activeModalWidget(): QWidgetH; cdecl; external QtIntf name 'QApplication_activeModalWidget';
function QApplication_clipboard(): QClipboardH; cdecl; external QtIntf name 'QApplication_clipboard';
function QApplication_focusWidget(): QWidgetH; cdecl; external QtIntf name 'QApplication_focusWidget';
function QApplication_activeWindow(): QWidgetH; cdecl; external QtIntf name 'QApplication_activeWindow';
procedure QApplication_setActiveWindow(act: QWidgetH); cdecl; external QtIntf name 'QApplication_setActiveWindow';
function QApplication_widgetAt(p: PQtPoint): QWidgetH; overload; cdecl; external QtIntf name 'QApplication_widgetAt';
function QApplication_widgetAt(x: Integer; y: Integer): QWidgetH; overload; cdecl; external QtIntf name 'QApplication_widgetAt2';
function QApplication_topLevelAt(p: PQtPoint): QWidgetH; overload; cdecl; external QtIntf name 'QApplication_topLevelAt';
function QApplication_topLevelAt(x: Integer; y: Integer): QWidgetH; overload; cdecl; external QtIntf name 'QApplication_topLevelAt2';
procedure QApplication_syncX(); cdecl; external QtIntf name 'QApplication_syncX';
procedure QApplication_beep(); cdecl; external QtIntf name 'QApplication_beep';
function QApplication_keyboardModifiers(): QtKeyboardModifiers; cdecl; external QtIntf name 'QApplication_keyboardModifiers';
function QApplication_mouseButtons(): QtMouseButtons; cdecl; external QtIntf name 'QApplication_mouseButtons';
procedure QApplication_setDesktopSettingsAware(p1: Boolean); cdecl; external QtIntf name 'QApplication_setDesktopSettingsAware';
function QApplication_desktopSettingsAware(): Boolean; cdecl; external QtIntf name 'QApplication_desktopSettingsAware';
procedure QApplication_setCursorFlashTime(p1: Integer); cdecl; external QtIntf name 'QApplication_setCursorFlashTime';
function QApplication_cursorFlashTime(): Integer; cdecl; external QtIntf name 'QApplication_cursorFlashTime';
procedure QApplication_setDoubleClickInterval(p1: Integer); cdecl; external QtIntf name 'QApplication_setDoubleClickInterval';
function QApplication_doubleClickInterval(): Integer; cdecl; external QtIntf name 'QApplication_doubleClickInterval';
procedure QApplication_setKeyboardInputInterval(p1: Integer); cdecl; external QtIntf name 'QApplication_setKeyboardInputInterval';
function QApplication_keyboardInputInterval(): Integer; cdecl; external QtIntf name 'QApplication_keyboardInputInterval';
procedure QApplication_setWheelScrollLines(p1: Integer); cdecl; external QtIntf name 'QApplication_setWheelScrollLines';
function QApplication_wheelScrollLines(): Integer; cdecl; external QtIntf name 'QApplication_wheelScrollLines';
procedure QApplication_setGlobalStrut(p1: PSize); cdecl; external QtIntf name 'QApplication_setGlobalStrut';
procedure QApplication_globalStrut(retval: PSize); cdecl; external QtIntf name 'QApplication_globalStrut';
procedure QApplication_setStartDragTime(ms: Integer); cdecl; external QtIntf name 'QApplication_setStartDragTime';
function QApplication_startDragTime(): Integer; cdecl; external QtIntf name 'QApplication_startDragTime';
procedure QApplication_setStartDragDistance(l: Integer); cdecl; external QtIntf name 'QApplication_setStartDragDistance';
function QApplication_startDragDistance(): Integer; cdecl; external QtIntf name 'QApplication_startDragDistance';
procedure QApplication_setLayoutDirection(direction: QtLayoutDirection); cdecl; external QtIntf name 'QApplication_setLayoutDirection';
function QApplication_layoutDirection(): QtLayoutDirection; cdecl; external QtIntf name 'QApplication_layoutDirection';
function QApplication_isRightToLeft(): Boolean; cdecl; external QtIntf name 'QApplication_isRightToLeft';
function QApplication_isLeftToRight(): Boolean; cdecl; external QtIntf name 'QApplication_isLeftToRight';
function QApplication_isEffectEnabled(p1: QtUIEffect): Boolean; cdecl; external QtIntf name 'QApplication_isEffectEnabled';
procedure QApplication_setEffectEnabled(p1: QtUIEffect; enable: Boolean = True); cdecl; external QtIntf name 'QApplication_setEffectEnabled';
{$ifdef UNIX }
function QApplication_x11EventFilter(handle: QApplicationH; p1: PEvent): Boolean; cdecl; external QtIntf name 'QApplication_x11EventFilter';
function QApplication_x11ClientMessage(handle: QApplicationH; p1: QWidgetH; p2: PEvent; passive_only: Boolean): Integer; cdecl; external QtIntf name 'QApplication_x11ClientMessage';
function QApplication_x11ProcessEvent(handle: QApplicationH; p1: PEvent): Integer; cdecl; external QtIntf name 'QApplication_x11ProcessEvent';
{$endif}
function QApplication_isSessionRestored(handle: QApplicationH): Boolean; cdecl; external QtIntf name 'QApplication_isSessionRestored';
procedure QApplication_sessionId(handle: QApplicationH; retval: PWideString); cdecl; external QtIntf name 'QApplication_sessionId';
procedure QApplication_sessionKey(handle: QApplicationH; retval: PWideString); cdecl; external QtIntf name 'QApplication_sessionKey';
procedure QApplication_commitData(handle: QApplicationH; sm: QSessionManagerH); cdecl; external QtIntf name 'QApplication_commitData';
procedure QApplication_saveState(handle: QApplicationH; sm: QSessionManagerH); cdecl; external QtIntf name 'QApplication_saveState';
procedure QApplication_setInputContext(handle: QApplicationH; p1: QInputContextH); cdecl; external QtIntf name 'QApplication_setInputContext';
function QApplication_inputContext(handle: QApplicationH): QInputContextH; cdecl; external QtIntf name 'QApplication_inputContext';
procedure QApplication_keyboardInputLocale(retval: QLocaleH); cdecl; external QtIntf name 'QApplication_keyboardInputLocale';
function QApplication_keyboardInputDirection(): QtLayoutDirection; cdecl; external QtIntf name 'QApplication_keyboardInputDirection';
function QApplication_exec(): Integer; cdecl; external QtIntf name 'QApplication_exec';
function QApplication_notify(handle: QApplicationH; p1: QObjectH; p2: QEventH): Boolean; cdecl; external QtIntf name 'QApplication_notify';
procedure QApplication_setQuitOnLastWindowClosed(quit: Boolean); cdecl; external QtIntf name 'QApplication_setQuitOnLastWindowClosed';
function QApplication_quitOnLastWindowClosed(): Boolean; cdecl; external QtIntf name 'QApplication_quitOnLastWindowClosed';
procedure QApplication_styleSheet(handle: QApplicationH; retval: PWideString); cdecl; external QtIntf name 'QApplication_styleSheet';
procedure QApplication_setStyleSheet(handle: QApplicationH; sheet: PWideString); cdecl; external QtIntf name 'QApplication_setStyleSheet';
procedure QApplication_closeAllWindows(); cdecl; external QtIntf name 'QApplication_closeAllWindows';
procedure QApplication_aboutQt(); cdecl; external QtIntf name 'QApplication_aboutQt';
{$ifdef MSWINDOWS }
procedure QApplication_winFocus(handle: QApplicationH; p1: QWidgetH; p2: Boolean); cdecl; external QtIntf name 'QApplication_winFocus';
procedure QApplication_winMouseButtonUp(); cdecl; external QtIntf name 'QApplication_winMouseButtonUp';
{$endif}
{$ifdef DARWIN }
function QApplication_macEventFilter(handle: QApplicationH; p1: EventHandlerCallRef; p2: EventRef): Boolean; cdecl; external QtIntf name 'QApplication_macEventFilter';
{$endif}


type
  QApplication_lastWindowClosed_Event = procedure () of object cdecl;
  QApplication_focusChanged_Event = procedure (old: QWidgetH; now: QWidgetH) of object cdecl;
  QApplication_commitDataRequest_Event = procedure (sessionManager: QSessionManagerH) of object cdecl;
  QApplication_saveStateRequest_Event = procedure (sessionManager: QSessionManagerH) of object cdecl;


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
    QPaletteNoRole = 17 { $11 };
    QPaletteNColorRoles = 17 { $11 };
    QPaletteForeground = 0 { $0 };
    QPaletteBackground = 10 { $a };


function QPalette_create(): QPaletteH; overload; cdecl; external QtIntf name 'QPalette_create';
procedure QPalette_destroy(handle: QPaletteH); cdecl; external QtIntf name 'QPalette_destroy'; 
function QPalette_create(button: PQColor): QPaletteH; overload; cdecl; external QtIntf name 'QPalette_create2';
function QPalette_create(button: QtGlobalColor): QPaletteH; overload; cdecl; external QtIntf name 'QPalette_create3';
function QPalette_create(button: PQColor; window: PQColor): QPaletteH; overload; cdecl; external QtIntf name 'QPalette_create4';
function QPalette_create(windowText: QBrushH; button: QBrushH; light: QBrushH; dark: QBrushH; mid: QBrushH; text: QBrushH; bright_text: QBrushH; base: QBrushH; window: QBrushH): QPaletteH; overload; cdecl; external QtIntf name 'QPalette_create5';
function QPalette_create(windowText: PQColor; window: PQColor; light: PQColor; dark: PQColor; mid: PQColor; text: PQColor; base: PQColor): QPaletteH; overload; cdecl; external QtIntf name 'QPalette_create6';
function QPalette_create(palette: QPaletteH): QPaletteH; overload; cdecl; external QtIntf name 'QPalette_create7';
function QPalette_currentColorGroup(handle: QPaletteH): QPaletteColorGroup; cdecl; external QtIntf name 'QPalette_currentColorGroup';
procedure QPalette_setCurrentColorGroup(handle: QPaletteH; cg: QPaletteColorGroup); cdecl; external QtIntf name 'QPalette_setCurrentColorGroup';
function QPalette_color(handle: QPaletteH; cg: QPaletteColorGroup; cr: QPaletteColorRole): PQColor; overload; cdecl; external QtIntf name 'QPalette_color';
function QPalette_brush(handle: QPaletteH; cg: QPaletteColorGroup; cr: QPaletteColorRole): QBrushH; overload; cdecl; external QtIntf name 'QPalette_brush';
procedure QPalette_setColor(handle: QPaletteH; cg: QPaletteColorGroup; cr: QPaletteColorRole; color: PQColor); overload; cdecl; external QtIntf name 'QPalette_setColor';
procedure QPalette_setColor(handle: QPaletteH; cr: QPaletteColorRole; color: PQColor); overload; cdecl; external QtIntf name 'QPalette_setColor2';
procedure QPalette_setBrush(handle: QPaletteH; cr: QPaletteColorRole; brush: QBrushH); overload; cdecl; external QtIntf name 'QPalette_setBrush';
function QPalette_isBrushSet(handle: QPaletteH; cg: QPaletteColorGroup; cr: QPaletteColorRole): Boolean; cdecl; external QtIntf name 'QPalette_isBrushSet';
procedure QPalette_setBrush(handle: QPaletteH; cg: QPaletteColorGroup; cr: QPaletteColorRole; brush: QBrushH); overload; cdecl; external QtIntf name 'QPalette_setBrush2';
procedure QPalette_setColorGroup(handle: QPaletteH; cr: QPaletteColorGroup; windowText: QBrushH; button: QBrushH; light: QBrushH; dark: QBrushH; mid: QBrushH; text: QBrushH; bright_text: QBrushH; base: QBrushH; window: QBrushH); cdecl; external QtIntf name 'QPalette_setColorGroup';
function QPalette_isEqual(handle: QPaletteH; cr1: QPaletteColorGroup; cr2: QPaletteColorGroup): Boolean; cdecl; external QtIntf name 'QPalette_isEqual';
function QPalette_color(handle: QPaletteH; cr: QPaletteColorRole): PQColor; overload; cdecl; external QtIntf name 'QPalette_color2';
function QPalette_brush(handle: QPaletteH; cr: QPaletteColorRole): QBrushH; overload; cdecl; external QtIntf name 'QPalette_brush2';
function QPalette_foreground(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_foreground';
function QPalette_windowText(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_windowText';
function QPalette_button(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_button';
function QPalette_light(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_light';
function QPalette_dark(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_dark';
function QPalette_mid(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_mid';
function QPalette_text(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_text';
function QPalette_base(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_base';
function QPalette_alternateBase(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_alternateBase';
function QPalette_background(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_background';
function QPalette_window(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_window';
function QPalette_midlight(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_midlight';
function QPalette_brightText(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_brightText';
function QPalette_buttonText(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_buttonText';
function QPalette_shadow(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_shadow';
function QPalette_highlight(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_highlight';
function QPalette_highlightedText(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_highlightedText';
function QPalette_link(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_link';
function QPalette_linkVisited(handle: QPaletteH): QBrushH; cdecl; external QtIntf name 'QPalette_linkVisited';
function QPalette_isCopyOf(handle: QPaletteH; p: QPaletteH): Boolean; cdecl; external QtIntf name 'QPalette_isCopyOf';
function QPalette_serialNumber(handle: QPaletteH): Integer; cdecl; external QtIntf name 'QPalette_serialNumber';
procedure QPalette_resolve(handle: QPaletteH; retval: QPaletteH; p1: QPaletteH); overload; cdecl; external QtIntf name 'QPalette_resolve';
function QPalette_resolve(handle: QPaletteH): LongWord; overload; cdecl; external QtIntf name 'QPalette_resolve2';
procedure QPalette_resolve(handle: QPaletteH; mask: LongWord); overload; cdecl; external QtIntf name 'QPalette_resolve3';


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
  QKeySequenceStandardKey = ( // QKeySequence::StandardKey (1)
    QKeySequenceUnknownKey, QKeySequenceHelpContents, QKeySequenceWhatsThis, QKeySequenceOpen, QKeySequenceClose, QKeySequenceSave, QKeySequenceNew, QKeySequenceDelete, QKeySequenceCut, 
    QKeySequenceCopy, QKeySequencePaste, QKeySequenceUndo, QKeySequenceRedo, QKeySequenceBack, QKeySequenceForward, QKeySequenceRefresh, QKeySequenceZoomIn, QKeySequenceZoomOut, QKeySequencePrint, 
    QKeySequenceAddTab, QKeySequenceNextChild, QKeySequencePreviousChild, QKeySequenceFind, QKeySequenceFindNext, QKeySequenceFindPrevious, QKeySequenceReplace, QKeySequenceSelectAll, QKeySequenceBold, 
    QKeySequenceItalic, QKeySequenceUnderline, QKeySequenceMoveToNextChar, QKeySequenceMoveToPreviousChar, QKeySequenceMoveToNextWord, QKeySequenceMoveToPreviousWord, QKeySequenceMoveToNextLine, 
    QKeySequenceMoveToPreviousLine, QKeySequenceMoveToNextPage, QKeySequenceMoveToPreviousPage, QKeySequenceMoveToStartOfLine, QKeySequenceMoveToEndOfLine, QKeySequenceMoveToStartOfBlock, 
    QKeySequenceMoveToEndOfBlock, QKeySequenceMoveToStartOfDocument, QKeySequenceMoveToEndOfDocument, QKeySequenceSelectNextChar, QKeySequenceSelectPreviousChar, QKeySequenceSelectNextWord, 
    QKeySequenceSelectPreviousWord, QKeySequenceSelectNextLine, QKeySequenceSelectPreviousLine, QKeySequenceSelectNextPage, QKeySequenceSelectPreviousPage, QKeySequenceSelectStartOfLine, 
    QKeySequenceSelectEndOfLine, QKeySequenceSelectStartOfBlock, QKeySequenceSelectEndOfBlock, QKeySequenceSelectStartOfDocument, QKeySequenceSelectEndOfDocument, QKeySequenceDeleteStartOfWord, 
    QKeySequenceDeleteEndOfWord, QKeySequenceDeleteEndOfLine );

  QKeySequenceSequenceMatch = ( // QKeySequence::SequenceMatch (1)
    QKeySequenceNoMatch, QKeySequencePartialMatch, QKeySequenceExactMatch );

  QKeySequenceSequenceFormat = ( // QKeySequence::SequenceFormat (1)
    QKeySequenceNativeText, QKeySequencePortableText );

function QKeySequence_create(): QKeySequenceH; overload; cdecl; external QtIntf name 'QKeySequence_create';
procedure QKeySequence_destroy(handle: QKeySequenceH); cdecl; external QtIntf name 'QKeySequence_destroy'; 
function QKeySequence_create(key: PWideString): QKeySequenceH; overload; cdecl; external QtIntf name 'QKeySequence_create2';
function QKeySequence_create(k1: Integer; k2: Integer = 0; k3: Integer = 0; k4: Integer = 0): QKeySequenceH; overload; cdecl; external QtIntf name 'QKeySequence_create3';
function QKeySequence_create(ks: QKeySequenceH): QKeySequenceH; overload; cdecl; external QtIntf name 'QKeySequence_create4';
function QKeySequence_create(key: QKeySequenceStandardKey): QKeySequenceH; overload; cdecl; external QtIntf name 'QKeySequence_create5';
function QKeySequence_count(handle: QKeySequenceH): LongWord; cdecl; external QtIntf name 'QKeySequence_count';
function QKeySequence_isEmpty(handle: QKeySequenceH): Boolean; cdecl; external QtIntf name 'QKeySequence_isEmpty';
procedure QKeySequence_toString(handle: QKeySequenceH; retval: PWideString; format: QKeySequenceSequenceFormat = QKeySequencePortableText); cdecl; external QtIntf name 'QKeySequence_toString';
procedure QKeySequence_fromString(retval: QKeySequenceH; str: PWideString; format: QKeySequenceSequenceFormat = QKeySequencePortableText); cdecl; external QtIntf name 'QKeySequence_fromString';
function QKeySequence_matches(handle: QKeySequenceH; seq: QKeySequenceH): QKeySequenceSequenceMatch; cdecl; external QtIntf name 'QKeySequence_matches';
procedure QKeySequence_mnemonic(retval: QKeySequenceH; text: PWideString); cdecl; external QtIntf name 'QKeySequence_mnemonic';
function QKeySequence_isDetached(handle: QKeySequenceH): Boolean; cdecl; external QtIntf name 'QKeySequence_isDetached';


function QWidget_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QWidgetH; cdecl; external QtIntf name 'QWidget_create';
procedure QWidget_destroy(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_destroy'; 
function QWidget_devType(handle: QWidgetH): Integer; cdecl; external QtIntf name 'QWidget_devType';
function QWidget_winId(handle: QWidgetH): LongWord; cdecl; external QtIntf name 'QWidget_winId';
procedure QWidget_createWinId(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_createWinId';
function QWidget_internalWinId(handle: QWidgetH): LongWord; cdecl; external QtIntf name 'QWidget_internalWinId';
function QWidget_style(handle: QWidgetH): QStyleH; cdecl; external QtIntf name 'QWidget_style';
procedure QWidget_setStyle(handle: QWidgetH; p1: QStyleH); cdecl; external QtIntf name 'QWidget_setStyle';
function QWidget_isTopLevel(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isTopLevel';
function QWidget_isWindow(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isWindow';
function QWidget_isModal(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isModal';
function QWidget_windowModality(handle: QWidgetH): QtWindowModality; cdecl; external QtIntf name 'QWidget_windowModality';
procedure QWidget_setWindowModality(handle: QWidgetH; windowModality: QtWindowModality); cdecl; external QtIntf name 'QWidget_setWindowModality';
function QWidget_isEnabled(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isEnabled';
function QWidget_isEnabledTo(handle: QWidgetH; p1: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isEnabledTo';
function QWidget_isEnabledToTLW(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isEnabledToTLW';
procedure QWidget_setEnabled(handle: QWidgetH; p1: Boolean); cdecl; external QtIntf name 'QWidget_setEnabled';
procedure QWidget_setDisabled(handle: QWidgetH; p1: Boolean); cdecl; external QtIntf name 'QWidget_setDisabled';
procedure QWidget_setWindowModified(handle: QWidgetH; p1: Boolean); cdecl; external QtIntf name 'QWidget_setWindowModified';
procedure QWidget_frameGeometry(handle: QWidgetH; retval: PRect); cdecl; external QtIntf name 'QWidget_frameGeometry';
procedure QWidget_geometry(handle: QWidgetH; retval: PRect); cdecl; external QtIntf name 'QWidget_geometry';
procedure QWidget_normalGeometry(handle: QWidgetH; retval: PRect); cdecl; external QtIntf name 'QWidget_normalGeometry';
function QWidget_x(handle: QWidgetH): Integer; cdecl; external QtIntf name 'QWidget_x';
function QWidget_y(handle: QWidgetH): Integer; cdecl; external QtIntf name 'QWidget_y';
procedure QWidget_pos(handle: QWidgetH; retval: PQtPoint); cdecl; external QtIntf name 'QWidget_pos';
procedure QWidget_frameSize(handle: QWidgetH; retval: PSize); cdecl; external QtIntf name 'QWidget_frameSize';
procedure QWidget_size(handle: QWidgetH; retval: PSize); cdecl; external QtIntf name 'QWidget_size';
function QWidget_width(handle: QWidgetH): Integer; cdecl; external QtIntf name 'QWidget_width';
function QWidget_height(handle: QWidgetH): Integer; cdecl; external QtIntf name 'QWidget_height';
procedure QWidget_rect(handle: QWidgetH; retval: PRect); cdecl; external QtIntf name 'QWidget_rect';
procedure QWidget_childrenRect(handle: QWidgetH; retval: PRect); cdecl; external QtIntf name 'QWidget_childrenRect';
procedure QWidget_childrenRegion(handle: QWidgetH; retval: QRegionH); cdecl; external QtIntf name 'QWidget_childrenRegion';
procedure QWidget_minimumSize(handle: QWidgetH; retval: PSize); cdecl; external QtIntf name 'QWidget_minimumSize';
procedure QWidget_maximumSize(handle: QWidgetH; retval: PSize); cdecl; external QtIntf name 'QWidget_maximumSize';
function QWidget_minimumWidth(handle: QWidgetH): Integer; cdecl; external QtIntf name 'QWidget_minimumWidth';
function QWidget_minimumHeight(handle: QWidgetH): Integer; cdecl; external QtIntf name 'QWidget_minimumHeight';
function QWidget_maximumWidth(handle: QWidgetH): Integer; cdecl; external QtIntf name 'QWidget_maximumWidth';
function QWidget_maximumHeight(handle: QWidgetH): Integer; cdecl; external QtIntf name 'QWidget_maximumHeight';
procedure QWidget_setMinimumSize(handle: QWidgetH; p1: PSize); overload; cdecl; external QtIntf name 'QWidget_setMinimumSize';
procedure QWidget_setMinimumSize(handle: QWidgetH; minw: Integer; minh: Integer); overload; cdecl; external QtIntf name 'QWidget_setMinimumSize2';
procedure QWidget_setMaximumSize(handle: QWidgetH; p1: PSize); overload; cdecl; external QtIntf name 'QWidget_setMaximumSize';
procedure QWidget_setMaximumSize(handle: QWidgetH; maxw: Integer; maxh: Integer); overload; cdecl; external QtIntf name 'QWidget_setMaximumSize2';
procedure QWidget_setMinimumWidth(handle: QWidgetH; minw: Integer); cdecl; external QtIntf name 'QWidget_setMinimumWidth';
procedure QWidget_setMinimumHeight(handle: QWidgetH; minh: Integer); cdecl; external QtIntf name 'QWidget_setMinimumHeight';
procedure QWidget_setMaximumWidth(handle: QWidgetH; maxw: Integer); cdecl; external QtIntf name 'QWidget_setMaximumWidth';
procedure QWidget_setMaximumHeight(handle: QWidgetH; maxh: Integer); cdecl; external QtIntf name 'QWidget_setMaximumHeight';
procedure QWidget_sizeIncrement(handle: QWidgetH; retval: PSize); cdecl; external QtIntf name 'QWidget_sizeIncrement';
procedure QWidget_setSizeIncrement(handle: QWidgetH; p1: PSize); overload; cdecl; external QtIntf name 'QWidget_setSizeIncrement';
procedure QWidget_setSizeIncrement(handle: QWidgetH; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QWidget_setSizeIncrement2';
procedure QWidget_baseSize(handle: QWidgetH; retval: PSize); cdecl; external QtIntf name 'QWidget_baseSize';
procedure QWidget_setBaseSize(handle: QWidgetH; p1: PSize); overload; cdecl; external QtIntf name 'QWidget_setBaseSize';
procedure QWidget_setBaseSize(handle: QWidgetH; basew: Integer; baseh: Integer); overload; cdecl; external QtIntf name 'QWidget_setBaseSize2';
procedure QWidget_setFixedSize(handle: QWidgetH; p1: PSize); overload; cdecl; external QtIntf name 'QWidget_setFixedSize';
procedure QWidget_setFixedSize(handle: QWidgetH; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QWidget_setFixedSize2';
procedure QWidget_setFixedWidth(handle: QWidgetH; w: Integer); cdecl; external QtIntf name 'QWidget_setFixedWidth';
procedure QWidget_setFixedHeight(handle: QWidgetH; h: Integer); cdecl; external QtIntf name 'QWidget_setFixedHeight';
procedure QWidget_mapToGlobal(handle: QWidgetH; retval: PQtPoint; p1: PQtPoint); cdecl; external QtIntf name 'QWidget_mapToGlobal';
procedure QWidget_mapFromGlobal(handle: QWidgetH; retval: PQtPoint; p1: PQtPoint); cdecl; external QtIntf name 'QWidget_mapFromGlobal';
procedure QWidget_mapToParent(handle: QWidgetH; retval: PQtPoint; p1: PQtPoint); cdecl; external QtIntf name 'QWidget_mapToParent';
procedure QWidget_mapFromParent(handle: QWidgetH; retval: PQtPoint; p1: PQtPoint); cdecl; external QtIntf name 'QWidget_mapFromParent';
procedure QWidget_mapTo(handle: QWidgetH; retval: PQtPoint; p1: QWidgetH; p2: PQtPoint); cdecl; external QtIntf name 'QWidget_mapTo';
procedure QWidget_mapFrom(handle: QWidgetH; retval: PQtPoint; p1: QWidgetH; p2: PQtPoint); cdecl; external QtIntf name 'QWidget_mapFrom';
function QWidget_window(handle: QWidgetH): QWidgetH; cdecl; external QtIntf name 'QWidget_window';
function QWidget_topLevelWidget(handle: QWidgetH): QWidgetH; cdecl; external QtIntf name 'QWidget_topLevelWidget';
function QWidget_palette(handle: QWidgetH): QPaletteH; cdecl; external QtIntf name 'QWidget_palette';
procedure QWidget_setPalette(handle: QWidgetH; p1: QPaletteH); cdecl; external QtIntf name 'QWidget_setPalette';
procedure QWidget_setBackgroundRole(handle: QWidgetH; p1: QPaletteColorRole); cdecl; external QtIntf name 'QWidget_setBackgroundRole';
function QWidget_backgroundRole(handle: QWidgetH): QPaletteColorRole; cdecl; external QtIntf name 'QWidget_backgroundRole';
procedure QWidget_setForegroundRole(handle: QWidgetH; p1: QPaletteColorRole); cdecl; external QtIntf name 'QWidget_setForegroundRole';
function QWidget_foregroundRole(handle: QWidgetH): QPaletteColorRole; cdecl; external QtIntf name 'QWidget_foregroundRole';
function QWidget_font(handle: QWidgetH): QFontH; cdecl; external QtIntf name 'QWidget_font';
procedure QWidget_setFont(handle: QWidgetH; p1: QFontH); cdecl; external QtIntf name 'QWidget_setFont';
procedure QWidget_fontMetrics(handle: QWidgetH; retval: QFontMetricsH); cdecl; external QtIntf name 'QWidget_fontMetrics';
procedure QWidget_fontInfo(handle: QWidgetH; retval: QFontInfoH); cdecl; external QtIntf name 'QWidget_fontInfo';
procedure QWidget_cursor(handle: QWidgetH; retval: QCursorH); cdecl; external QtIntf name 'QWidget_cursor';
procedure QWidget_setCursor(handle: QWidgetH; p1: QCursorH); cdecl; external QtIntf name 'QWidget_setCursor';
procedure QWidget_unsetCursor(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_unsetCursor';
procedure QWidget_setMouseTracking(handle: QWidgetH; enable: Boolean); cdecl; external QtIntf name 'QWidget_setMouseTracking';
function QWidget_hasMouseTracking(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_hasMouseTracking';
function QWidget_underMouse(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_underMouse';
procedure QWidget_setMask(handle: QWidgetH; p1: QBitmapH); overload; cdecl; external QtIntf name 'QWidget_setMask';
procedure QWidget_setMask(handle: QWidgetH; p1: QRegionH); overload; cdecl; external QtIntf name 'QWidget_setMask2';
procedure QWidget_mask(handle: QWidgetH; retval: QRegionH); cdecl; external QtIntf name 'QWidget_mask';
procedure QWidget_clearMask(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_clearMask';
procedure QWidget_setWindowTitle(handle: QWidgetH; p1: PWideString); cdecl; external QtIntf name 'QWidget_setWindowTitle';
procedure QWidget_setStyleSheet(handle: QWidgetH; styleSheet: PWideString); cdecl; external QtIntf name 'QWidget_setStyleSheet';
procedure QWidget_styleSheet(handle: QWidgetH; retval: PWideString); cdecl; external QtIntf name 'QWidget_styleSheet';
procedure QWidget_windowTitle(handle: QWidgetH; retval: PWideString); cdecl; external QtIntf name 'QWidget_windowTitle';
procedure QWidget_setWindowIcon(handle: QWidgetH; icon: QIconH); cdecl; external QtIntf name 'QWidget_setWindowIcon';
procedure QWidget_windowIcon(handle: QWidgetH; retval: QIconH); cdecl; external QtIntf name 'QWidget_windowIcon';
procedure QWidget_setWindowIconText(handle: QWidgetH; p1: PWideString); cdecl; external QtIntf name 'QWidget_setWindowIconText';
procedure QWidget_windowIconText(handle: QWidgetH; retval: PWideString); cdecl; external QtIntf name 'QWidget_windowIconText';
procedure QWidget_setWindowRole(handle: QWidgetH; p1: PWideString); cdecl; external QtIntf name 'QWidget_setWindowRole';
procedure QWidget_windowRole(handle: QWidgetH; retval: PWideString); cdecl; external QtIntf name 'QWidget_windowRole';
procedure QWidget_setWindowOpacity(handle: QWidgetH; level: Double); cdecl; external QtIntf name 'QWidget_setWindowOpacity';
function QWidget_windowOpacity(handle: QWidgetH): Double; cdecl; external QtIntf name 'QWidget_windowOpacity';
function QWidget_isWindowModified(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isWindowModified';
procedure QWidget_setToolTip(handle: QWidgetH; p1: PWideString); cdecl; external QtIntf name 'QWidget_setToolTip';
procedure QWidget_toolTip(handle: QWidgetH; retval: PWideString); cdecl; external QtIntf name 'QWidget_toolTip';
procedure QWidget_setStatusTip(handle: QWidgetH; p1: PWideString); cdecl; external QtIntf name 'QWidget_setStatusTip';
procedure QWidget_statusTip(handle: QWidgetH; retval: PWideString); cdecl; external QtIntf name 'QWidget_statusTip';
procedure QWidget_setWhatsThis(handle: QWidgetH; p1: PWideString); cdecl; external QtIntf name 'QWidget_setWhatsThis';
procedure QWidget_whatsThis(handle: QWidgetH; retval: PWideString); cdecl; external QtIntf name 'QWidget_whatsThis';
procedure QWidget_accessibleName(handle: QWidgetH; retval: PWideString); cdecl; external QtIntf name 'QWidget_accessibleName';
procedure QWidget_setAccessibleName(handle: QWidgetH; name: PWideString); cdecl; external QtIntf name 'QWidget_setAccessibleName';
procedure QWidget_accessibleDescription(handle: QWidgetH; retval: PWideString); cdecl; external QtIntf name 'QWidget_accessibleDescription';
procedure QWidget_setAccessibleDescription(handle: QWidgetH; description: PWideString); cdecl; external QtIntf name 'QWidget_setAccessibleDescription';
procedure QWidget_setLayoutDirection(handle: QWidgetH; direction: QtLayoutDirection); cdecl; external QtIntf name 'QWidget_setLayoutDirection';
function QWidget_layoutDirection(handle: QWidgetH): QtLayoutDirection; cdecl; external QtIntf name 'QWidget_layoutDirection';
procedure QWidget_unsetLayoutDirection(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_unsetLayoutDirection';
function QWidget_isRightToLeft(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isRightToLeft';
function QWidget_isLeftToRight(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isLeftToRight';
procedure QWidget_setFocus(handle: QWidgetH); overload; cdecl; external QtIntf name 'QWidget_setFocus';
function QWidget_isActiveWindow(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isActiveWindow';
procedure QWidget_activateWindow(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_activateWindow';
procedure QWidget_clearFocus(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_clearFocus';
procedure QWidget_setFocus(handle: QWidgetH; reason: QtFocusReason); overload; cdecl; external QtIntf name 'QWidget_setFocus2';
function QWidget_focusPolicy(handle: QWidgetH): QtFocusPolicy; cdecl; external QtIntf name 'QWidget_focusPolicy';
procedure QWidget_setFocusPolicy(handle: QWidgetH; policy: QtFocusPolicy); cdecl; external QtIntf name 'QWidget_setFocusPolicy';
function QWidget_hasFocus(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_hasFocus';
procedure QWidget_setTabOrder(p1: QWidgetH; p2: QWidgetH); cdecl; external QtIntf name 'QWidget_setTabOrder';
procedure QWidget_setFocusProxy(handle: QWidgetH; p1: QWidgetH); cdecl; external QtIntf name 'QWidget_setFocusProxy';
function QWidget_focusProxy(handle: QWidgetH): QWidgetH; cdecl; external QtIntf name 'QWidget_focusProxy';
function QWidget_contextMenuPolicy(handle: QWidgetH): QtContextMenuPolicy; cdecl; external QtIntf name 'QWidget_contextMenuPolicy';
procedure QWidget_setContextMenuPolicy(handle: QWidgetH; policy: QtContextMenuPolicy); cdecl; external QtIntf name 'QWidget_setContextMenuPolicy';
procedure QWidget_grabMouse(handle: QWidgetH); overload; cdecl; external QtIntf name 'QWidget_grabMouse';
procedure QWidget_grabMouse(handle: QWidgetH; p1: QCursorH); overload; cdecl; external QtIntf name 'QWidget_grabMouse2';
procedure QWidget_releaseMouse(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_releaseMouse';
procedure QWidget_grabKeyboard(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_grabKeyboard';
procedure QWidget_releaseKeyboard(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_releaseKeyboard';
function QWidget_grabShortcut(handle: QWidgetH; key: QKeySequenceH; context: QtShortcutContext = QtWindowShortcut): Integer; cdecl; external QtIntf name 'QWidget_grabShortcut';
procedure QWidget_releaseShortcut(handle: QWidgetH; id: Integer); cdecl; external QtIntf name 'QWidget_releaseShortcut';
procedure QWidget_setShortcutEnabled(handle: QWidgetH; id: Integer; enable: Boolean = True); cdecl; external QtIntf name 'QWidget_setShortcutEnabled';
procedure QWidget_setShortcutAutoRepeat(handle: QWidgetH; id: Integer; enable: Boolean = True); cdecl; external QtIntf name 'QWidget_setShortcutAutoRepeat';
function QWidget_mouseGrabber(): QWidgetH; cdecl; external QtIntf name 'QWidget_mouseGrabber';
function QWidget_keyboardGrabber(): QWidgetH; cdecl; external QtIntf name 'QWidget_keyboardGrabber';
function QWidget_updatesEnabled(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_updatesEnabled';
procedure QWidget_setUpdatesEnabled(handle: QWidgetH; enable: Boolean); cdecl; external QtIntf name 'QWidget_setUpdatesEnabled';
procedure QWidget_update(handle: QWidgetH); overload; cdecl; external QtIntf name 'QWidget_update';
procedure QWidget_repaint(handle: QWidgetH); overload; cdecl; external QtIntf name 'QWidget_repaint';
procedure QWidget_update(handle: QWidgetH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QWidget_update2';
procedure QWidget_update(handle: QWidgetH; p1: PRect); overload; cdecl; external QtIntf name 'QWidget_update3';
procedure QWidget_update(handle: QWidgetH; p1: QRegionH); overload; cdecl; external QtIntf name 'QWidget_update4';
procedure QWidget_repaint(handle: QWidgetH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QWidget_repaint2';
procedure QWidget_repaint(handle: QWidgetH; p1: PRect); overload; cdecl; external QtIntf name 'QWidget_repaint3';
procedure QWidget_repaint(handle: QWidgetH; p1: QRegionH); overload; cdecl; external QtIntf name 'QWidget_repaint4';
procedure QWidget_setVisible(handle: QWidgetH; visible: Boolean); cdecl; external QtIntf name 'QWidget_setVisible';
procedure QWidget_setHidden(handle: QWidgetH; hidden: Boolean); cdecl; external QtIntf name 'QWidget_setHidden';
procedure QWidget_show(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_show';
procedure QWidget_hide(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_hide';
procedure QWidget_setShown(handle: QWidgetH; shown: Boolean); cdecl; external QtIntf name 'QWidget_setShown';
procedure QWidget_showMinimized(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_showMinimized';
procedure QWidget_showMaximized(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_showMaximized';
procedure QWidget_showFullScreen(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_showFullScreen';
procedure QWidget_showNormal(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_showNormal';
function QWidget_close(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_close';
procedure QWidget_raise(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_raise';
procedure QWidget_lower(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_lower';
procedure QWidget_stackUnder(handle: QWidgetH; p1: QWidgetH); cdecl; external QtIntf name 'QWidget_stackUnder';
procedure QWidget_move(handle: QWidgetH; x: Integer; y: Integer); overload; cdecl; external QtIntf name 'QWidget_move';
procedure QWidget_move(handle: QWidgetH; p1: PQtPoint); overload; cdecl; external QtIntf name 'QWidget_move2';
procedure QWidget_resize(handle: QWidgetH; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QWidget_resize';
procedure QWidget_resize(handle: QWidgetH; p1: PSize); overload; cdecl; external QtIntf name 'QWidget_resize2';
procedure QWidget_setGeometry(handle: QWidgetH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QWidget_setGeometry';
procedure QWidget_setGeometry(handle: QWidgetH; p1: PRect); overload; cdecl; external QtIntf name 'QWidget_setGeometry2';
procedure QWidget_saveGeometry(handle: QWidgetH; retval: QByteArrayH); cdecl; external QtIntf name 'QWidget_saveGeometry';
function QWidget_restoreGeometry(handle: QWidgetH; geometry: QByteArrayH): Boolean; cdecl; external QtIntf name 'QWidget_restoreGeometry';
procedure QWidget_adjustSize(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_adjustSize';
function QWidget_isVisible(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isVisible';
function QWidget_isVisibleTo(handle: QWidgetH; p1: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isVisibleTo';
function QWidget_isHidden(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isHidden';
function QWidget_isMinimized(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isMinimized';
function QWidget_isMaximized(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isMaximized';
function QWidget_isFullScreen(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isFullScreen';
function QWidget_windowState(handle: QWidgetH): QtWindowStates; cdecl; external QtIntf name 'QWidget_windowState';
procedure QWidget_setWindowState(handle: QWidgetH; state: QtWindowStates); cdecl; external QtIntf name 'QWidget_setWindowState';
procedure QWidget_overrideWindowState(handle: QWidgetH; state: QtWindowStates); cdecl; external QtIntf name 'QWidget_overrideWindowState';
procedure QWidget_sizeHint(handle: QWidgetH; retval: PSize); cdecl; external QtIntf name 'QWidget_sizeHint';
procedure QWidget_minimumSizeHint(handle: QWidgetH; retval: PSize); cdecl; external QtIntf name 'QWidget_minimumSizeHint';
procedure QWidget_sizePolicy(handle: QWidgetH; retval: PSizePolicy); cdecl; external QtIntf name 'QWidget_sizePolicy';
procedure QWidget_setSizePolicy(handle: QWidgetH; p1: PSizePolicy); overload; cdecl; external QtIntf name 'QWidget_setSizePolicy';
procedure QWidget_setSizePolicy(handle: QWidgetH; horizontal: QSizePolicyPolicy; vertical: QSizePolicyPolicy); overload; cdecl; external QtIntf name 'QWidget_setSizePolicy2';
function QWidget_heightForWidth(handle: QWidgetH; p1: Integer): Integer; cdecl; external QtIntf name 'QWidget_heightForWidth';
procedure QWidget_visibleRegion(handle: QWidgetH; retval: QRegionH); cdecl; external QtIntf name 'QWidget_visibleRegion';
procedure QWidget_setContentsMargins(handle: QWidgetH; left: Integer; top: Integer; right: Integer; bottom: Integer); cdecl; external QtIntf name 'QWidget_setContentsMargins';
procedure QWidget_getContentsMargins(handle: QWidgetH; left: PInteger; top: PInteger; right: PInteger; bottom: PInteger); cdecl; external QtIntf name 'QWidget_getContentsMargins';
procedure QWidget_contentsRect(handle: QWidgetH; retval: PRect); cdecl; external QtIntf name 'QWidget_contentsRect';
function QWidget_layout(handle: QWidgetH): QLayoutH; cdecl; external QtIntf name 'QWidget_layout';
procedure QWidget_setLayout(handle: QWidgetH; p1: QLayoutH); cdecl; external QtIntf name 'QWidget_setLayout';
procedure QWidget_updateGeometry(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_updateGeometry';
procedure QWidget_setParent(handle: QWidgetH; parent: QWidgetH); overload; cdecl; external QtIntf name 'QWidget_setParent';
procedure QWidget_setParent(handle: QWidgetH; parent: QWidgetH; f: QtWindowFlags); overload; cdecl; external QtIntf name 'QWidget_setParent2';
procedure QWidget_scroll(handle: QWidgetH; dx: Integer; dy: Integer); overload; cdecl; external QtIntf name 'QWidget_scroll';
procedure QWidget_scroll(handle: QWidgetH; dx: Integer; dy: Integer; p3: PRect); overload; cdecl; external QtIntf name 'QWidget_scroll2';
function QWidget_focusWidget(handle: QWidgetH): QWidgetH; cdecl; external QtIntf name 'QWidget_focusWidget';
function QWidget_nextInFocusChain(handle: QWidgetH): QWidgetH; cdecl; external QtIntf name 'QWidget_nextInFocusChain';
function QWidget_acceptDrops(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_acceptDrops';
procedure QWidget_setAcceptDrops(handle: QWidgetH; _on: Boolean); cdecl; external QtIntf name 'QWidget_setAcceptDrops';
procedure QWidget_addAction(handle: QWidgetH; action: QActionH); cdecl; external QtIntf name 'QWidget_addAction';
procedure QWidget_addActions(handle: QWidgetH; actions: PIntArray); cdecl; external QtIntf name 'QWidget_addActions';
procedure QWidget_insertAction(handle: QWidgetH; before: QActionH; action: QActionH); cdecl; external QtIntf name 'QWidget_insertAction';
procedure QWidget_insertActions(handle: QWidgetH; before: QActionH; actions: PIntArray); cdecl; external QtIntf name 'QWidget_insertActions';
procedure QWidget_removeAction(handle: QWidgetH; action: QActionH); cdecl; external QtIntf name 'QWidget_removeAction';
procedure QWidget_actions(handle: QWidgetH; retval: PIntArray); cdecl; external QtIntf name 'QWidget_actions';
function QWidget_parentWidget(handle: QWidgetH): QWidgetH; cdecl; external QtIntf name 'QWidget_parentWidget';
procedure QWidget_setWindowFlags(handle: QWidgetH; _type: QtWindowFlags); cdecl; external QtIntf name 'QWidget_setWindowFlags';
function QWidget_windowFlags(handle: QWidgetH): QtWindowFlags; cdecl; external QtIntf name 'QWidget_windowFlags';
procedure QWidget_overrideWindowFlags(handle: QWidgetH; _type: QtWindowFlags); cdecl; external QtIntf name 'QWidget_overrideWindowFlags';
function QWidget_windowType(handle: QWidgetH): QtWindowType; cdecl; external QtIntf name 'QWidget_windowType';
function QWidget_find(p1: LongWord): QWidgetH; cdecl; external QtIntf name 'QWidget_find';
function QWidget_childAt(handle: QWidgetH; x: Integer; y: Integer): QWidgetH; overload; cdecl; external QtIntf name 'QWidget_childAt';
function QWidget_childAt(handle: QWidgetH; p: PQtPoint): QWidgetH; overload; cdecl; external QtIntf name 'QWidget_childAt2';
{$ifdef UNIX }
function QWidget_x11Info(handle: QWidgetH): QX11InfoH; cdecl; external QtIntf name 'QWidget_x11Info';
function QWidget_x11PictureHandle(handle: QWidgetH): QtHANDLE; cdecl; external QtIntf name 'QWidget_x11PictureHandle';
{$endif}
{$ifdef UNIX or DARWIN }
function QWidget_handle(handle: QWidgetH): QtHANDLE; cdecl; external QtIntf name 'QWidget_handle';
{$endif}
procedure QWidget_setAttribute(handle: QWidgetH; p1: QtWidgetAttribute; _on: Boolean = True); cdecl; external QtIntf name 'QWidget_setAttribute';
function QWidget_testAttribute(handle: QWidgetH; p1: QtWidgetAttribute): Boolean; cdecl; external QtIntf name 'QWidget_testAttribute';
function QWidget_paintEngine(handle: QWidgetH): QPaintEngineH; cdecl; external QtIntf name 'QWidget_paintEngine';
procedure QWidget_ensurePolished(handle: QWidgetH); cdecl; external QtIntf name 'QWidget_ensurePolished';
function QWidget_inputContext(handle: QWidgetH): QInputContextH; cdecl; external QtIntf name 'QWidget_inputContext';
procedure QWidget_setInputContext(handle: QWidgetH; p1: QInputContextH); cdecl; external QtIntf name 'QWidget_setInputContext';
function QWidget_isAncestorOf(handle: QWidgetH; child: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_isAncestorOf';
function QWidget_autoFillBackground(handle: QWidgetH): Boolean; cdecl; external QtIntf name 'QWidget_autoFillBackground';
procedure QWidget_setAutoFillBackground(handle: QWidgetH; enabled: Boolean); cdecl; external QtIntf name 'QWidget_setAutoFillBackground';
procedure QWidget_inputMethodQuery(handle: QWidgetH; retval: QVariantH; p1: QtInputMethodQuery); cdecl; external QtIntf name 'QWidget_inputMethodQuery';
{$ifdef MSWINDOWS }
function QWidget_getDC(handle: QWidgetH): HDC; cdecl; external QtIntf name 'QWidget_getDC';
procedure QWidget_releaseDC(handle: QWidgetH; p1: HDC); cdecl; external QtIntf name 'QWidget_releaseDC';
{$endif}
function QWidget_to_QPaintDevice(handle: QWidgetH): QPaintDeviceH; cdecl; external QtIntf name 'QWidget_to_QPaintDevice';

type
  QWidget_customContextMenuRequested_Event = procedure (pos: PQtPoint) of object cdecl;


procedure QLayoutItem_sizeHint(handle: QLayoutItemH; retval: PSize); cdecl; external QtIntf name 'QLayoutItem_sizeHint';
procedure QLayoutItem_minimumSize(handle: QLayoutItemH; retval: PSize); cdecl; external QtIntf name 'QLayoutItem_minimumSize';
procedure QLayoutItem_maximumSize(handle: QLayoutItemH; retval: PSize); cdecl; external QtIntf name 'QLayoutItem_maximumSize';
function QLayoutItem_expandingDirections(handle: QLayoutItemH): QtOrientations; cdecl; external QtIntf name 'QLayoutItem_expandingDirections';
procedure QLayoutItem_setGeometry(handle: QLayoutItemH; p1: PRect); cdecl; external QtIntf name 'QLayoutItem_setGeometry';
procedure QLayoutItem_geometry(handle: QLayoutItemH; retval: PRect); cdecl; external QtIntf name 'QLayoutItem_geometry';
function QLayoutItem_isEmpty(handle: QLayoutItemH): Boolean; cdecl; external QtIntf name 'QLayoutItem_isEmpty';
function QLayoutItem_hasHeightForWidth(handle: QLayoutItemH): Boolean; cdecl; external QtIntf name 'QLayoutItem_hasHeightForWidth';
function QLayoutItem_heightForWidth(handle: QLayoutItemH; p1: Integer): Integer; cdecl; external QtIntf name 'QLayoutItem_heightForWidth';
function QLayoutItem_minimumHeightForWidth(handle: QLayoutItemH; p1: Integer): Integer; cdecl; external QtIntf name 'QLayoutItem_minimumHeightForWidth';
procedure QLayoutItem_invalidate(handle: QLayoutItemH); cdecl; external QtIntf name 'QLayoutItem_invalidate';
function QLayoutItem_widget(handle: QLayoutItemH): QWidgetH; cdecl; external QtIntf name 'QLayoutItem_widget';
function QLayoutItem_layout(handle: QLayoutItemH): QLayoutH; cdecl; external QtIntf name 'QLayoutItem_layout';
function QLayoutItem_spacerItem(handle: QLayoutItemH): QSpacerItemH; cdecl; external QtIntf name 'QLayoutItem_spacerItem';
function QLayoutItem_alignment(handle: QLayoutItemH): QtAlignment; cdecl; external QtIntf name 'QLayoutItem_alignment';
procedure QLayoutItem_setAlignment(handle: QLayoutItemH; a: QtAlignment); cdecl; external QtIntf name 'QLayoutItem_setAlignment';

function QSpacerItem_create(w: Integer; h: Integer; hData: QSizePolicyPolicy = QSizePolicyMinimum; vData: QSizePolicyPolicy = QSizePolicyMinimum): QSpacerItemH; cdecl; external QtIntf name 'QSpacerItem_create';
procedure QSpacerItem_destroy(handle: QSpacerItemH); cdecl; external QtIntf name 'QSpacerItem_destroy'; 
procedure QSpacerItem_changeSize(handle: QSpacerItemH; w: Integer; h: Integer; hData: QSizePolicyPolicy = QSizePolicyMinimum; vData: QSizePolicyPolicy = QSizePolicyMinimum); cdecl; external QtIntf name 'QSpacerItem_changeSize';
procedure QSpacerItem_sizeHint(handle: QSpacerItemH; retval: PSize); cdecl; external QtIntf name 'QSpacerItem_sizeHint';
procedure QSpacerItem_minimumSize(handle: QSpacerItemH; retval: PSize); cdecl; external QtIntf name 'QSpacerItem_minimumSize';
procedure QSpacerItem_maximumSize(handle: QSpacerItemH; retval: PSize); cdecl; external QtIntf name 'QSpacerItem_maximumSize';
function QSpacerItem_expandingDirections(handle: QSpacerItemH): QtOrientations; cdecl; external QtIntf name 'QSpacerItem_expandingDirections';
function QSpacerItem_isEmpty(handle: QSpacerItemH): Boolean; cdecl; external QtIntf name 'QSpacerItem_isEmpty';
procedure QSpacerItem_setGeometry(handle: QSpacerItemH; p1: PRect); cdecl; external QtIntf name 'QSpacerItem_setGeometry';
procedure QSpacerItem_geometry(handle: QSpacerItemH; retval: PRect); cdecl; external QtIntf name 'QSpacerItem_geometry';
function QSpacerItem_spacerItem(handle: QSpacerItemH): QSpacerItemH; cdecl; external QtIntf name 'QSpacerItem_spacerItem';

function QWidgetItem_create(w: QWidgetH): QWidgetItemH; cdecl; external QtIntf name 'QWidgetItem_create';
procedure QWidgetItem_destroy(handle: QWidgetItemH); cdecl; external QtIntf name 'QWidgetItem_destroy'; 
procedure QWidgetItem_sizeHint(handle: QWidgetItemH; retval: PSize); cdecl; external QtIntf name 'QWidgetItem_sizeHint';
procedure QWidgetItem_minimumSize(handle: QWidgetItemH; retval: PSize); cdecl; external QtIntf name 'QWidgetItem_minimumSize';
procedure QWidgetItem_maximumSize(handle: QWidgetItemH; retval: PSize); cdecl; external QtIntf name 'QWidgetItem_maximumSize';
function QWidgetItem_expandingDirections(handle: QWidgetItemH): QtOrientations; cdecl; external QtIntf name 'QWidgetItem_expandingDirections';
function QWidgetItem_isEmpty(handle: QWidgetItemH): Boolean; cdecl; external QtIntf name 'QWidgetItem_isEmpty';
procedure QWidgetItem_setGeometry(handle: QWidgetItemH; p1: PRect); cdecl; external QtIntf name 'QWidgetItem_setGeometry';
procedure QWidgetItem_geometry(handle: QWidgetItemH; retval: PRect); cdecl; external QtIntf name 'QWidgetItem_geometry';
function QWidgetItem_widget(handle: QWidgetItemH): QWidgetH; cdecl; external QtIntf name 'QWidgetItem_widget';
function QWidgetItem_hasHeightForWidth(handle: QWidgetItemH): Boolean; cdecl; external QtIntf name 'QWidgetItem_hasHeightForWidth';
function QWidgetItem_heightForWidth(handle: QWidgetItemH; p1: Integer): Integer; cdecl; external QtIntf name 'QWidgetItem_heightForWidth';


type
  QLayoutSizeConstraint = ( // QLayout::SizeConstraint (1)
    QLayoutSetDefaultConstraint, QLayoutSetNoConstraint, QLayoutSetMinimumSize, QLayoutSetFixedSize, QLayoutSetMaximumSize, QLayoutSetMinAndMaxSize );

function QLayout_margin(handle: QLayoutH): Integer; cdecl; external QtIntf name 'QLayout_margin';
function QLayout_spacing(handle: QLayoutH): Integer; cdecl; external QtIntf name 'QLayout_spacing';
procedure QLayout_setMargin(handle: QLayoutH; p1: Integer); cdecl; external QtIntf name 'QLayout_setMargin';
procedure QLayout_setSpacing(handle: QLayoutH; p1: Integer); cdecl; external QtIntf name 'QLayout_setSpacing';
function QLayout_setAlignment(handle: QLayoutH; w: QWidgetH; alignment: QtAlignment): Boolean; overload; cdecl; external QtIntf name 'QLayout_setAlignment';
function QLayout_setAlignment(handle: QLayoutH; l: QLayoutH; alignment: QtAlignment): Boolean; overload; cdecl; external QtIntf name 'QLayout_setAlignment2';
procedure QLayout_setSizeConstraint(handle: QLayoutH; p1: QLayoutSizeConstraint); cdecl; external QtIntf name 'QLayout_setSizeConstraint';
function QLayout_sizeConstraint(handle: QLayoutH): QLayoutSizeConstraint; cdecl; external QtIntf name 'QLayout_sizeConstraint';
procedure QLayout_setMenuBar(handle: QLayoutH; w: QWidgetH); cdecl; external QtIntf name 'QLayout_setMenuBar';
function QLayout_menuBar(handle: QLayoutH): QWidgetH; cdecl; external QtIntf name 'QLayout_menuBar';
function QLayout_parentWidget(handle: QLayoutH): QWidgetH; cdecl; external QtIntf name 'QLayout_parentWidget';
procedure QLayout_invalidate(handle: QLayoutH); cdecl; external QtIntf name 'QLayout_invalidate';
procedure QLayout_geometry(handle: QLayoutH; retval: PRect); cdecl; external QtIntf name 'QLayout_geometry';
function QLayout_activate(handle: QLayoutH): Boolean; cdecl; external QtIntf name 'QLayout_activate';
procedure QLayout_update(handle: QLayoutH); cdecl; external QtIntf name 'QLayout_update';
procedure QLayout_addWidget(handle: QLayoutH; w: QWidgetH); cdecl; external QtIntf name 'QLayout_addWidget';
procedure QLayout_addItem(handle: QLayoutH; p1: QLayoutItemH); cdecl; external QtIntf name 'QLayout_addItem';
procedure QLayout_removeWidget(handle: QLayoutH; w: QWidgetH); cdecl; external QtIntf name 'QLayout_removeWidget';
procedure QLayout_removeItem(handle: QLayoutH; p1: QLayoutItemH); cdecl; external QtIntf name 'QLayout_removeItem';
function QLayout_expandingDirections(handle: QLayoutH): QtOrientations; cdecl; external QtIntf name 'QLayout_expandingDirections';
procedure QLayout_minimumSize(handle: QLayoutH; retval: PSize); cdecl; external QtIntf name 'QLayout_minimumSize';
procedure QLayout_maximumSize(handle: QLayoutH; retval: PSize); cdecl; external QtIntf name 'QLayout_maximumSize';
procedure QLayout_setGeometry(handle: QLayoutH; p1: PRect); cdecl; external QtIntf name 'QLayout_setGeometry';
function QLayout_itemAt(handle: QLayoutH; index: Integer): QLayoutItemH; cdecl; external QtIntf name 'QLayout_itemAt';
function QLayout_takeAt(handle: QLayoutH; index: Integer): QLayoutItemH; cdecl; external QtIntf name 'QLayout_takeAt';
function QLayout_indexOf(handle: QLayoutH; p1: QWidgetH): Integer; cdecl; external QtIntf name 'QLayout_indexOf';
function QLayout_count(handle: QLayoutH): Integer; cdecl; external QtIntf name 'QLayout_count';
function QLayout_isEmpty(handle: QLayoutH): Boolean; cdecl; external QtIntf name 'QLayout_isEmpty';
function QLayout_totalHeightForWidth(handle: QLayoutH; w: Integer): Integer; cdecl; external QtIntf name 'QLayout_totalHeightForWidth';
procedure QLayout_totalMinimumSize(handle: QLayoutH; retval: PSize); cdecl; external QtIntf name 'QLayout_totalMinimumSize';
procedure QLayout_totalMaximumSize(handle: QLayoutH; retval: PSize); cdecl; external QtIntf name 'QLayout_totalMaximumSize';
procedure QLayout_totalSizeHint(handle: QLayoutH; retval: PSize); cdecl; external QtIntf name 'QLayout_totalSizeHint';
function QLayout_layout(handle: QLayoutH): QLayoutH; cdecl; external QtIntf name 'QLayout_layout';
procedure QLayout_setEnabled(handle: QLayoutH; p1: Boolean); cdecl; external QtIntf name 'QLayout_setEnabled';
function QLayout_isEnabled(handle: QLayoutH): Boolean; cdecl; external QtIntf name 'QLayout_isEnabled';
procedure QLayout_closestAcceptableSize(retval: PSize; w: QWidgetH; s: PSize); cdecl; external QtIntf name 'QLayout_closestAcceptableSize';
function QLayout_to_QLayoutItem(handle: QLayoutH): QLayoutItemH; cdecl; external QtIntf name 'QLayout_to_QLayoutItem';
type
  QBoxLayoutDirection = cardinal; //  QBoxLayout::Direction (4)

const
    QBoxLayoutLeftToRight = 0 { $0 };
    QBoxLayoutRightToLeft = 1 { $1 };
    QBoxLayoutTopToBottom = 2 { $2 };
    QBoxLayoutBottomToTop = 3 { $3 };
    QBoxLayoutDown = 2 { $2 };
    QBoxLayoutUp = 3 { $3 };


function QBoxLayout_create(p1: QBoxLayoutDirection; parent: QWidgetH = nil): QBoxLayoutH; cdecl; external QtIntf name 'QBoxLayout_create';
procedure QBoxLayout_destroy(handle: QBoxLayoutH); cdecl; external QtIntf name 'QBoxLayout_destroy'; 
function QBoxLayout_direction(handle: QBoxLayoutH): QBoxLayoutDirection; cdecl; external QtIntf name 'QBoxLayout_direction';
procedure QBoxLayout_setDirection(handle: QBoxLayoutH; p1: QBoxLayoutDirection); cdecl; external QtIntf name 'QBoxLayout_setDirection';
procedure QBoxLayout_addSpacing(handle: QBoxLayoutH; size: Integer); cdecl; external QtIntf name 'QBoxLayout_addSpacing';
procedure QBoxLayout_addStretch(handle: QBoxLayoutH; stretch: Integer = 0); cdecl; external QtIntf name 'QBoxLayout_addStretch';
procedure QBoxLayout_addWidget(handle: QBoxLayoutH; p1: QWidgetH; stretch: Integer = 0; alignment: QtAlignment = 0); cdecl; external QtIntf name 'QBoxLayout_addWidget';
procedure QBoxLayout_addLayout(handle: QBoxLayoutH; layout: QLayoutH; stretch: Integer = 0); cdecl; external QtIntf name 'QBoxLayout_addLayout';
procedure QBoxLayout_addStrut(handle: QBoxLayoutH; p1: Integer); cdecl; external QtIntf name 'QBoxLayout_addStrut';
procedure QBoxLayout_addItem(handle: QBoxLayoutH; p1: QLayoutItemH); cdecl; external QtIntf name 'QBoxLayout_addItem';
procedure QBoxLayout_insertSpacing(handle: QBoxLayoutH; index: Integer; size: Integer); cdecl; external QtIntf name 'QBoxLayout_insertSpacing';
procedure QBoxLayout_insertStretch(handle: QBoxLayoutH; index: Integer; stretch: Integer = 0); cdecl; external QtIntf name 'QBoxLayout_insertStretch';
procedure QBoxLayout_insertWidget(handle: QBoxLayoutH; index: Integer; widget: QWidgetH; stretch: Integer = 0; alignment: QtAlignment = 0); cdecl; external QtIntf name 'QBoxLayout_insertWidget';
procedure QBoxLayout_insertLayout(handle: QBoxLayoutH; index: Integer; layout: QLayoutH; stretch: Integer = 0); cdecl; external QtIntf name 'QBoxLayout_insertLayout';
function QBoxLayout_setStretchFactor(handle: QBoxLayoutH; w: QWidgetH; stretch: Integer): Boolean; overload; cdecl; external QtIntf name 'QBoxLayout_setStretchFactor';
function QBoxLayout_setStretchFactor(handle: QBoxLayoutH; l: QLayoutH; stretch: Integer): Boolean; overload; cdecl; external QtIntf name 'QBoxLayout_setStretchFactor2';
procedure QBoxLayout_sizeHint(handle: QBoxLayoutH; retval: PSize); cdecl; external QtIntf name 'QBoxLayout_sizeHint';
procedure QBoxLayout_minimumSize(handle: QBoxLayoutH; retval: PSize); cdecl; external QtIntf name 'QBoxLayout_minimumSize';
procedure QBoxLayout_maximumSize(handle: QBoxLayoutH; retval: PSize); cdecl; external QtIntf name 'QBoxLayout_maximumSize';
function QBoxLayout_hasHeightForWidth(handle: QBoxLayoutH): Boolean; cdecl; external QtIntf name 'QBoxLayout_hasHeightForWidth';
function QBoxLayout_heightForWidth(handle: QBoxLayoutH; p1: Integer): Integer; cdecl; external QtIntf name 'QBoxLayout_heightForWidth';
function QBoxLayout_minimumHeightForWidth(handle: QBoxLayoutH; p1: Integer): Integer; cdecl; external QtIntf name 'QBoxLayout_minimumHeightForWidth';
function QBoxLayout_expandingDirections(handle: QBoxLayoutH): QtOrientations; cdecl; external QtIntf name 'QBoxLayout_expandingDirections';
procedure QBoxLayout_invalidate(handle: QBoxLayoutH); cdecl; external QtIntf name 'QBoxLayout_invalidate';
function QBoxLayout_itemAt(handle: QBoxLayoutH; p1: Integer): QLayoutItemH; cdecl; external QtIntf name 'QBoxLayout_itemAt';
function QBoxLayout_takeAt(handle: QBoxLayoutH; p1: Integer): QLayoutItemH; cdecl; external QtIntf name 'QBoxLayout_takeAt';
function QBoxLayout_count(handle: QBoxLayoutH): Integer; cdecl; external QtIntf name 'QBoxLayout_count';
procedure QBoxLayout_setGeometry(handle: QBoxLayoutH; p1: PRect); cdecl; external QtIntf name 'QBoxLayout_setGeometry';

function QHBoxLayout_create(): QHBoxLayoutH; overload; cdecl; external QtIntf name 'QHBoxLayout_create';
procedure QHBoxLayout_destroy(handle: QHBoxLayoutH); cdecl; external QtIntf name 'QHBoxLayout_destroy'; 
function QHBoxLayout_create(parent: QWidgetH): QHBoxLayoutH; overload; cdecl; external QtIntf name 'QHBoxLayout_create2';

function QVBoxLayout_create(): QVBoxLayoutH; overload; cdecl; external QtIntf name 'QVBoxLayout_create';
procedure QVBoxLayout_destroy(handle: QVBoxLayoutH); cdecl; external QtIntf name 'QVBoxLayout_destroy'; 
function QVBoxLayout_create(parent: QWidgetH): QVBoxLayoutH; overload; cdecl; external QtIntf name 'QVBoxLayout_create2';


type
  QActionMenuRole = ( // QAction::MenuRole (1)
    QActionNoRole, QActionTextHeuristicRole, QActionApplicationSpecificRole, QActionAboutQtRole, QActionAboutRole, QActionPreferencesRole, QActionQuitRole );

  QActionActionEvent = ( // QAction::ActionEvent (1)
    QActionTrigger, QActionHover );

function QAction_create(parent: QObjectH): QActionH; overload; cdecl; external QtIntf name 'QAction_create';
procedure QAction_destroy(handle: QActionH); cdecl; external QtIntf name 'QAction_destroy'; 
function QAction_create(text: PWideString; parent: QObjectH): QActionH; overload; cdecl; external QtIntf name 'QAction_create2';
function QAction_create(icon: QIconH; text: PWideString; parent: QObjectH): QActionH; overload; cdecl; external QtIntf name 'QAction_create3';
procedure QAction_setActionGroup(handle: QActionH; group: QActionGroupH); cdecl; external QtIntf name 'QAction_setActionGroup';
function QAction_actionGroup(handle: QActionH): QActionGroupH; cdecl; external QtIntf name 'QAction_actionGroup';
procedure QAction_setIcon(handle: QActionH; icon: QIconH); cdecl; external QtIntf name 'QAction_setIcon';
procedure QAction_icon(handle: QActionH; retval: QIconH); cdecl; external QtIntf name 'QAction_icon';
procedure QAction_setText(handle: QActionH; text: PWideString); cdecl; external QtIntf name 'QAction_setText';
procedure QAction_text(handle: QActionH; retval: PWideString); cdecl; external QtIntf name 'QAction_text';
procedure QAction_setIconText(handle: QActionH; text: PWideString); cdecl; external QtIntf name 'QAction_setIconText';
procedure QAction_iconText(handle: QActionH; retval: PWideString); cdecl; external QtIntf name 'QAction_iconText';
procedure QAction_setToolTip(handle: QActionH; tip: PWideString); cdecl; external QtIntf name 'QAction_setToolTip';
procedure QAction_toolTip(handle: QActionH; retval: PWideString); cdecl; external QtIntf name 'QAction_toolTip';
procedure QAction_setStatusTip(handle: QActionH; statusTip: PWideString); cdecl; external QtIntf name 'QAction_setStatusTip';
procedure QAction_statusTip(handle: QActionH; retval: PWideString); cdecl; external QtIntf name 'QAction_statusTip';
procedure QAction_setWhatsThis(handle: QActionH; what: PWideString); cdecl; external QtIntf name 'QAction_setWhatsThis';
procedure QAction_whatsThis(handle: QActionH; retval: PWideString); cdecl; external QtIntf name 'QAction_whatsThis';
function QAction_menu(handle: QActionH): QMenuH; cdecl; external QtIntf name 'QAction_menu';
procedure QAction_setMenu(handle: QActionH; menu: QMenuH); cdecl; external QtIntf name 'QAction_setMenu';
procedure QAction_setSeparator(handle: QActionH; b: Boolean); cdecl; external QtIntf name 'QAction_setSeparator';
function QAction_isSeparator(handle: QActionH): Boolean; cdecl; external QtIntf name 'QAction_isSeparator';
procedure QAction_setShortcut(handle: QActionH; shortcut: QKeySequenceH); cdecl; external QtIntf name 'QAction_setShortcut';
procedure QAction_shortcut(handle: QActionH; retval: QKeySequenceH); cdecl; external QtIntf name 'QAction_shortcut';
procedure QAction_setShortcuts(handle: QActionH; p1: QKeySequenceStandardKey); overload; cdecl; external QtIntf name 'QAction_setShortcuts2';
procedure QAction_setShortcutContext(handle: QActionH; context: QtShortcutContext); cdecl; external QtIntf name 'QAction_setShortcutContext';
function QAction_shortcutContext(handle: QActionH): QtShortcutContext; cdecl; external QtIntf name 'QAction_shortcutContext';
procedure QAction_setAutoRepeat(handle: QActionH; p1: Boolean); cdecl; external QtIntf name 'QAction_setAutoRepeat';
function QAction_autoRepeat(handle: QActionH): Boolean; cdecl; external QtIntf name 'QAction_autoRepeat';
procedure QAction_setFont(handle: QActionH; font: QFontH); cdecl; external QtIntf name 'QAction_setFont';
procedure QAction_font(handle: QActionH; retval: QFontH); cdecl; external QtIntf name 'QAction_font';
procedure QAction_setCheckable(handle: QActionH; p1: Boolean); cdecl; external QtIntf name 'QAction_setCheckable';
function QAction_isCheckable(handle: QActionH): Boolean; cdecl; external QtIntf name 'QAction_isCheckable';
procedure QAction_data(handle: QActionH; retval: QVariantH); cdecl; external QtIntf name 'QAction_data';
procedure QAction_setData(handle: QActionH; _var: QVariantH); cdecl; external QtIntf name 'QAction_setData';
function QAction_isChecked(handle: QActionH): Boolean; cdecl; external QtIntf name 'QAction_isChecked';
function QAction_isEnabled(handle: QActionH): Boolean; cdecl; external QtIntf name 'QAction_isEnabled';
function QAction_isVisible(handle: QActionH): Boolean; cdecl; external QtIntf name 'QAction_isVisible';
procedure QAction_activate(handle: QActionH; event: QActionActionEvent); cdecl; external QtIntf name 'QAction_activate';
function QAction_showStatusText(handle: QActionH; widget: QWidgetH = nil): Boolean; cdecl; external QtIntf name 'QAction_showStatusText';
procedure QAction_setMenuRole(handle: QActionH; menuRole: QActionMenuRole); cdecl; external QtIntf name 'QAction_setMenuRole';
function QAction_menuRole(handle: QActionH): QActionMenuRole; cdecl; external QtIntf name 'QAction_menuRole';
function QAction_parentWidget(handle: QActionH): QWidgetH; cdecl; external QtIntf name 'QAction_parentWidget';
procedure QAction_trigger(handle: QActionH); cdecl; external QtIntf name 'QAction_trigger';
procedure QAction_hover(handle: QActionH); cdecl; external QtIntf name 'QAction_hover';
procedure QAction_setChecked(handle: QActionH; p1: Boolean); cdecl; external QtIntf name 'QAction_setChecked';
procedure QAction_toggle(handle: QActionH); cdecl; external QtIntf name 'QAction_toggle';
procedure QAction_setEnabled(handle: QActionH; p1: Boolean); cdecl; external QtIntf name 'QAction_setEnabled';
procedure QAction_setDisabled(handle: QActionH; b: Boolean); cdecl; external QtIntf name 'QAction_setDisabled';
procedure QAction_setVisible(handle: QActionH; p1: Boolean); cdecl; external QtIntf name 'QAction_setVisible';


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

function QInputEvent_create(_type: QEventType; modifiers: QtKeyboardModifiers = QtNoModifier): QInputEventH; cdecl; external QtIntf name 'QInputEvent_create';
procedure QInputEvent_destroy(handle: QInputEventH); cdecl; external QtIntf name 'QInputEvent_destroy'; 
function QInputEvent_modifiers(handle: QInputEventH): QtKeyboardModifiers; cdecl; external QtIntf name 'QInputEvent_modifiers';

function QMouseEvent_create(_type: QEventType; pos: PQtPoint; button: QtMouseButton; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers): QMouseEventH; overload; cdecl; external QtIntf name 'QMouseEvent_create';
procedure QMouseEvent_destroy(handle: QMouseEventH); cdecl; external QtIntf name 'QMouseEvent_destroy'; 
function QMouseEvent_create(_type: QEventType; pos: PQtPoint; globalPos: PQtPoint; button: QtMouseButton; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers): QMouseEventH; overload; cdecl; external QtIntf name 'QMouseEvent_create2';
function QMouseEvent_pos(handle: QMouseEventH): PQtPoint; cdecl; external QtIntf name 'QMouseEvent_pos';
function QMouseEvent_globalPos(handle: QMouseEventH): PQtPoint; cdecl; external QtIntf name 'QMouseEvent_globalPos';
function QMouseEvent_x(handle: QMouseEventH): Integer; cdecl; external QtIntf name 'QMouseEvent_x';
function QMouseEvent_y(handle: QMouseEventH): Integer; cdecl; external QtIntf name 'QMouseEvent_y';
function QMouseEvent_globalX(handle: QMouseEventH): Integer; cdecl; external QtIntf name 'QMouseEvent_globalX';
function QMouseEvent_globalY(handle: QMouseEventH): Integer; cdecl; external QtIntf name 'QMouseEvent_globalY';
function QMouseEvent_button(handle: QMouseEventH): QtMouseButton; cdecl; external QtIntf name 'QMouseEvent_button';
function QMouseEvent_buttons(handle: QMouseEventH): QtMouseButtons; cdecl; external QtIntf name 'QMouseEvent_buttons';

function QHoverEvent_create(_type: QEventType; pos: PQtPoint; oldPos: PQtPoint): QHoverEventH; cdecl; external QtIntf name 'QHoverEvent_create';
procedure QHoverEvent_destroy(handle: QHoverEventH); cdecl; external QtIntf name 'QHoverEvent_destroy'; 
function QHoverEvent_pos(handle: QHoverEventH): PQtPoint; cdecl; external QtIntf name 'QHoverEvent_pos';
function QHoverEvent_oldPos(handle: QHoverEventH): PQtPoint; cdecl; external QtIntf name 'QHoverEvent_oldPos';

function QWheelEvent_create(pos: PQtPoint; delta: Integer; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers; orient: QtOrientation = QtVertical): QWheelEventH; overload; cdecl; external QtIntf name 'QWheelEvent_create';
procedure QWheelEvent_destroy(handle: QWheelEventH); cdecl; external QtIntf name 'QWheelEvent_destroy'; 
function QWheelEvent_create(pos: PQtPoint; globalPos: PQtPoint; delta: Integer; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers; orient: QtOrientation = QtVertical): QWheelEventH; overload; cdecl; external QtIntf name 'QWheelEvent_create2';
function QWheelEvent_delta(handle: QWheelEventH): Integer; cdecl; external QtIntf name 'QWheelEvent_delta';
function QWheelEvent_pos(handle: QWheelEventH): PQtPoint; cdecl; external QtIntf name 'QWheelEvent_pos';
function QWheelEvent_globalPos(handle: QWheelEventH): PQtPoint; cdecl; external QtIntf name 'QWheelEvent_globalPos';
function QWheelEvent_x(handle: QWheelEventH): Integer; cdecl; external QtIntf name 'QWheelEvent_x';
function QWheelEvent_y(handle: QWheelEventH): Integer; cdecl; external QtIntf name 'QWheelEvent_y';
function QWheelEvent_globalX(handle: QWheelEventH): Integer; cdecl; external QtIntf name 'QWheelEvent_globalX';
function QWheelEvent_globalY(handle: QWheelEventH): Integer; cdecl; external QtIntf name 'QWheelEvent_globalY';
function QWheelEvent_buttons(handle: QWheelEventH): QtMouseButtons; cdecl; external QtIntf name 'QWheelEvent_buttons';
function QWheelEvent_orientation(handle: QWheelEventH): QtOrientation; cdecl; external QtIntf name 'QWheelEvent_orientation';

function QTabletEvent_create(t: QEventType; pos: PQtPoint; globalPos: PQtPoint; hiResGlobalPos: QPointFH; device: Integer; pointerType: Integer; pressure: Double; xTilt: Integer; yTilt: Integer; tangentialPressure: Double; rotation: Double; z: Integer; keyState: QtKeyboardModifiers; uniqueID: int64): QTabletEventH; cdecl; external QtIntf name 'QTabletEvent_create';
procedure QTabletEvent_destroy(handle: QTabletEventH); cdecl; external QtIntf name 'QTabletEvent_destroy'; 
function QTabletEvent_pos(handle: QTabletEventH): PQtPoint; cdecl; external QtIntf name 'QTabletEvent_pos';
function QTabletEvent_globalPos(handle: QTabletEventH): PQtPoint; cdecl; external QtIntf name 'QTabletEvent_globalPos';
function QTabletEvent_hiResGlobalPos(handle: QTabletEventH): QPointFH; cdecl; external QtIntf name 'QTabletEvent_hiResGlobalPos';
function QTabletEvent_x(handle: QTabletEventH): Integer; cdecl; external QtIntf name 'QTabletEvent_x';
function QTabletEvent_y(handle: QTabletEventH): Integer; cdecl; external QtIntf name 'QTabletEvent_y';
function QTabletEvent_globalX(handle: QTabletEventH): Integer; cdecl; external QtIntf name 'QTabletEvent_globalX';
function QTabletEvent_globalY(handle: QTabletEventH): Integer; cdecl; external QtIntf name 'QTabletEvent_globalY';
function QTabletEvent_hiResGlobalX(handle: QTabletEventH): Double; cdecl; external QtIntf name 'QTabletEvent_hiResGlobalX';
function QTabletEvent_hiResGlobalY(handle: QTabletEventH): Double; cdecl; external QtIntf name 'QTabletEvent_hiResGlobalY';
function QTabletEvent_device(handle: QTabletEventH): QTabletEventTabletDevice; cdecl; external QtIntf name 'QTabletEvent_device';
function QTabletEvent_pointerType(handle: QTabletEventH): QTabletEventPointerType; cdecl; external QtIntf name 'QTabletEvent_pointerType';
function QTabletEvent_uniqueId(handle: QTabletEventH): int64; cdecl; external QtIntf name 'QTabletEvent_uniqueId';
function QTabletEvent_pressure(handle: QTabletEventH): Double; cdecl; external QtIntf name 'QTabletEvent_pressure';
function QTabletEvent_z(handle: QTabletEventH): Integer; cdecl; external QtIntf name 'QTabletEvent_z';
function QTabletEvent_tangentialPressure(handle: QTabletEventH): Double; cdecl; external QtIntf name 'QTabletEvent_tangentialPressure';
function QTabletEvent_rotation(handle: QTabletEventH): Double; cdecl; external QtIntf name 'QTabletEvent_rotation';
function QTabletEvent_xTilt(handle: QTabletEventH): Integer; cdecl; external QtIntf name 'QTabletEvent_xTilt';
function QTabletEvent_yTilt(handle: QTabletEventH): Integer; cdecl; external QtIntf name 'QTabletEvent_yTilt';

function QKeyEvent_create(_type: QEventType; key: Integer; modifiers: QtKeyboardModifiers; text: PWideString = nil; autorep: Boolean = False; count: Word = 1): QKeyEventH; cdecl; external QtIntf name 'QKeyEvent_create';
procedure QKeyEvent_destroy(handle: QKeyEventH); cdecl; external QtIntf name 'QKeyEvent_destroy'; 
function QKeyEvent_key(handle: QKeyEventH): Integer; cdecl; external QtIntf name 'QKeyEvent_key';
function QKeyEvent_matches(handle: QKeyEventH; key: QKeySequenceStandardKey): Boolean; cdecl; external QtIntf name 'QKeyEvent_matches';
function QKeyEvent_modifiers(handle: QKeyEventH): QtKeyboardModifiers; cdecl; external QtIntf name 'QKeyEvent_modifiers';
procedure QKeyEvent_text(handle: QKeyEventH; retval: PWideString); cdecl; external QtIntf name 'QKeyEvent_text';
function QKeyEvent_isAutoRepeat(handle: QKeyEventH): Boolean; cdecl; external QtIntf name 'QKeyEvent_isAutoRepeat';
function QKeyEvent_count(handle: QKeyEventH): Integer; cdecl; external QtIntf name 'QKeyEvent_count';
function QKeyEvent_createExtendedKeyEvent(_type: QEventType; key: Integer; modifiers: QtKeyboardModifiers; nativeScanCode: LongWord; nativeVirtualKey: LongWord; nativeModifiers: LongWord; text: PWideString = nil; autorep: Boolean = False; count: Word = 1): QKeyEventH; cdecl; external QtIntf name 'QKeyEvent_createExtendedKeyEvent';
function QKeyEvent_hasExtendedInfo(handle: QKeyEventH): Boolean; cdecl; external QtIntf name 'QKeyEvent_hasExtendedInfo';
function QKeyEvent_nativeScanCode(handle: QKeyEventH): LongWord; cdecl; external QtIntf name 'QKeyEvent_nativeScanCode';
function QKeyEvent_nativeVirtualKey(handle: QKeyEventH): LongWord; cdecl; external QtIntf name 'QKeyEvent_nativeVirtualKey';
function QKeyEvent_nativeModifiers(handle: QKeyEventH): LongWord; cdecl; external QtIntf name 'QKeyEvent_nativeModifiers';

function QFocusEvent_create(_type: QEventType; reason: QtFocusReason = QtOtherFocusReason): QFocusEventH; cdecl; external QtIntf name 'QFocusEvent_create';
procedure QFocusEvent_destroy(handle: QFocusEventH); cdecl; external QtIntf name 'QFocusEvent_destroy'; 
function QFocusEvent_gotFocus(handle: QFocusEventH): Boolean; cdecl; external QtIntf name 'QFocusEvent_gotFocus';
function QFocusEvent_lostFocus(handle: QFocusEventH): Boolean; cdecl; external QtIntf name 'QFocusEvent_lostFocus';
function QFocusEvent_reason(handle: QFocusEventH): QtFocusReason; cdecl; external QtIntf name 'QFocusEvent_reason';

function QPaintEvent_create(paintRegion: QRegionH): QPaintEventH; overload; cdecl; external QtIntf name 'QPaintEvent_create';
procedure QPaintEvent_destroy(handle: QPaintEventH); cdecl; external QtIntf name 'QPaintEvent_destroy'; 
function QPaintEvent_create(paintRect: PRect): QPaintEventH; overload; cdecl; external QtIntf name 'QPaintEvent_create2';
procedure QPaintEvent_rect(handle: QPaintEventH; retval: PRect); cdecl; external QtIntf name 'QPaintEvent_rect';
function QPaintEvent_region(handle: QPaintEventH): QRegionH; cdecl; external QtIntf name 'QPaintEvent_region';

function QMoveEvent_create(pos: PQtPoint; oldPos: PQtPoint): QMoveEventH; cdecl; external QtIntf name 'QMoveEvent_create';
procedure QMoveEvent_destroy(handle: QMoveEventH); cdecl; external QtIntf name 'QMoveEvent_destroy'; 
function QMoveEvent_pos(handle: QMoveEventH): PQtPoint; cdecl; external QtIntf name 'QMoveEvent_pos';
function QMoveEvent_oldPos(handle: QMoveEventH): PQtPoint; cdecl; external QtIntf name 'QMoveEvent_oldPos';

function QResizeEvent_create(size: PSize; oldSize: PSize): QResizeEventH; cdecl; external QtIntf name 'QResizeEvent_create';
procedure QResizeEvent_destroy(handle: QResizeEventH); cdecl; external QtIntf name 'QResizeEvent_destroy'; 
function QResizeEvent_size(handle: QResizeEventH): PSize; cdecl; external QtIntf name 'QResizeEvent_size';
function QResizeEvent_oldSize(handle: QResizeEventH): PSize; cdecl; external QtIntf name 'QResizeEvent_oldSize';

function QCloseEvent_create(): QCloseEventH; cdecl; external QtIntf name 'QCloseEvent_create';
procedure QCloseEvent_destroy(handle: QCloseEventH); cdecl; external QtIntf name 'QCloseEvent_destroy'; 

function QIconDragEvent_create(): QIconDragEventH; cdecl; external QtIntf name 'QIconDragEvent_create';
procedure QIconDragEvent_destroy(handle: QIconDragEventH); cdecl; external QtIntf name 'QIconDragEvent_destroy'; 

function QShowEvent_create(): QShowEventH; cdecl; external QtIntf name 'QShowEvent_create';
procedure QShowEvent_destroy(handle: QShowEventH); cdecl; external QtIntf name 'QShowEvent_destroy'; 

function QHideEvent_create(): QHideEventH; cdecl; external QtIntf name 'QHideEvent_create';
procedure QHideEvent_destroy(handle: QHideEventH); cdecl; external QtIntf name 'QHideEvent_destroy'; 

function QContextMenuEvent_create(reason: QContextMenuEventReason; pos: PQtPoint; globalPos: PQtPoint): QContextMenuEventH; overload; cdecl; external QtIntf name 'QContextMenuEvent_create';
procedure QContextMenuEvent_destroy(handle: QContextMenuEventH); cdecl; external QtIntf name 'QContextMenuEvent_destroy'; 
function QContextMenuEvent_create(reason: QContextMenuEventReason; pos: PQtPoint): QContextMenuEventH; overload; cdecl; external QtIntf name 'QContextMenuEvent_create2';
function QContextMenuEvent_x(handle: QContextMenuEventH): Integer; cdecl; external QtIntf name 'QContextMenuEvent_x';
function QContextMenuEvent_y(handle: QContextMenuEventH): Integer; cdecl; external QtIntf name 'QContextMenuEvent_y';
function QContextMenuEvent_globalX(handle: QContextMenuEventH): Integer; cdecl; external QtIntf name 'QContextMenuEvent_globalX';
function QContextMenuEvent_globalY(handle: QContextMenuEventH): Integer; cdecl; external QtIntf name 'QContextMenuEvent_globalY';
function QContextMenuEvent_pos(handle: QContextMenuEventH): PQtPoint; cdecl; external QtIntf name 'QContextMenuEvent_pos';
function QContextMenuEvent_globalPos(handle: QContextMenuEventH): PQtPoint; cdecl; external QtIntf name 'QContextMenuEvent_globalPos';
function QContextMenuEvent_reason(handle: QContextMenuEventH): QContextMenuEventReason; cdecl; external QtIntf name 'QContextMenuEvent_reason';

function QInputMethodEvent_create(): QInputMethodEventH; overload; cdecl; external QtIntf name 'QInputMethodEvent_create';
procedure QInputMethodEvent_destroy(handle: QInputMethodEventH); cdecl; external QtIntf name 'QInputMethodEvent_destroy'; 
procedure QInputMethodEvent_setCommitString(handle: QInputMethodEventH; commitString: PWideString; replaceFrom: Integer = 0; replaceLength: Integer = 0); cdecl; external QtIntf name 'QInputMethodEvent_setCommitString';
procedure QInputMethodEvent_preeditString(handle: QInputMethodEventH; retval: PWideString); cdecl; external QtIntf name 'QInputMethodEvent_preeditString';
procedure QInputMethodEvent_commitString(handle: QInputMethodEventH; retval: PWideString); cdecl; external QtIntf name 'QInputMethodEvent_commitString';
function QInputMethodEvent_replacementStart(handle: QInputMethodEventH): Integer; cdecl; external QtIntf name 'QInputMethodEvent_replacementStart';
function QInputMethodEvent_replacementLength(handle: QInputMethodEventH): Integer; cdecl; external QtIntf name 'QInputMethodEvent_replacementLength';
function QInputMethodEvent_create(other: QInputMethodEventH): QInputMethodEventH; overload; cdecl; external QtIntf name 'QInputMethodEvent_create3';

function QDropEvent_create(pos: PQtPoint; actions: QtDropActions; data: QMimeDataH; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers; _type: QEventType): QDropEventH; cdecl; external QtIntf name 'QDropEvent_create';
procedure QDropEvent_destroy(handle: QDropEventH); cdecl; external QtIntf name 'QDropEvent_destroy'; 
function QDropEvent_pos(handle: QDropEventH): PQtPoint; cdecl; external QtIntf name 'QDropEvent_pos';
function QDropEvent_mouseButtons(handle: QDropEventH): QtMouseButtons; cdecl; external QtIntf name 'QDropEvent_mouseButtons';
function QDropEvent_keyboardModifiers(handle: QDropEventH): QtKeyboardModifiers; cdecl; external QtIntf name 'QDropEvent_keyboardModifiers';
function QDropEvent_possibleActions(handle: QDropEventH): QtDropActions; cdecl; external QtIntf name 'QDropEvent_possibleActions';
function QDropEvent_proposedAction(handle: QDropEventH): QtDropAction; cdecl; external QtIntf name 'QDropEvent_proposedAction';
procedure QDropEvent_acceptProposedAction(handle: QDropEventH); cdecl; external QtIntf name 'QDropEvent_acceptProposedAction';
function QDropEvent_dropAction(handle: QDropEventH): QtDropAction; cdecl; external QtIntf name 'QDropEvent_dropAction';
procedure QDropEvent_setDropAction(handle: QDropEventH; action: QtDropAction); cdecl; external QtIntf name 'QDropEvent_setDropAction';
function QDropEvent_source(handle: QDropEventH): QWidgetH; cdecl; external QtIntf name 'QDropEvent_source';
function QDropEvent_mimeData(handle: QDropEventH): QMimeDataH; cdecl; external QtIntf name 'QDropEvent_mimeData';
function QDropEvent_format(handle: QDropEventH; n: Integer = 0): PAnsiChar; cdecl; external QtIntf name 'QDropEvent_format';
procedure QDropEvent_encodedData(handle: QDropEventH; retval: QByteArrayH; p1: PAnsiChar); cdecl; external QtIntf name 'QDropEvent_encodedData';
function QDropEvent_provides(handle: QDropEventH; p1: PAnsiChar): Boolean; cdecl; external QtIntf name 'QDropEvent_provides';

function QDragMoveEvent_create(pos: PQtPoint; actions: QtDropActions; data: QMimeDataH; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers; _type: QEventType): QDragMoveEventH; cdecl; external QtIntf name 'QDragMoveEvent_create';
procedure QDragMoveEvent_destroy(handle: QDragMoveEventH); cdecl; external QtIntf name 'QDragMoveEvent_destroy'; 
procedure QDragMoveEvent_answerRect(handle: QDragMoveEventH; retval: PRect); cdecl; external QtIntf name 'QDragMoveEvent_answerRect';
procedure QDragMoveEvent_accept(handle: QDragMoveEventH); overload; cdecl; external QtIntf name 'QDragMoveEvent_accept';
procedure QDragMoveEvent_ignore(handle: QDragMoveEventH); overload; cdecl; external QtIntf name 'QDragMoveEvent_ignore';
procedure QDragMoveEvent_accept(handle: QDragMoveEventH; r: PRect); overload; cdecl; external QtIntf name 'QDragMoveEvent_accept2';
procedure QDragMoveEvent_ignore(handle: QDragMoveEventH; r: PRect); overload; cdecl; external QtIntf name 'QDragMoveEvent_ignore2';

function QDragEnterEvent_create(pos: PQtPoint; actions: QtDropActions; data: QMimeDataH; buttons: QtMouseButtons; modifiers: QtKeyboardModifiers): QDragEnterEventH; cdecl; external QtIntf name 'QDragEnterEvent_create';
procedure QDragEnterEvent_destroy(handle: QDragEnterEventH); cdecl; external QtIntf name 'QDragEnterEvent_destroy'; 

function QDragLeaveEvent_create(): QDragLeaveEventH; cdecl; external QtIntf name 'QDragLeaveEvent_create';
procedure QDragLeaveEvent_destroy(handle: QDragLeaveEventH); cdecl; external QtIntf name 'QDragLeaveEvent_destroy'; 

function QHelpEvent_create(_type: QEventType; pos: PQtPoint; globalPos: PQtPoint): QHelpEventH; cdecl; external QtIntf name 'QHelpEvent_create';
procedure QHelpEvent_destroy(handle: QHelpEventH); cdecl; external QtIntf name 'QHelpEvent_destroy'; 
function QHelpEvent_x(handle: QHelpEventH): Integer; cdecl; external QtIntf name 'QHelpEvent_x';
function QHelpEvent_y(handle: QHelpEventH): Integer; cdecl; external QtIntf name 'QHelpEvent_y';
function QHelpEvent_globalX(handle: QHelpEventH): Integer; cdecl; external QtIntf name 'QHelpEvent_globalX';
function QHelpEvent_globalY(handle: QHelpEventH): Integer; cdecl; external QtIntf name 'QHelpEvent_globalY';
function QHelpEvent_pos(handle: QHelpEventH): PQtPoint; cdecl; external QtIntf name 'QHelpEvent_pos';
function QHelpEvent_globalPos(handle: QHelpEventH): PQtPoint; cdecl; external QtIntf name 'QHelpEvent_globalPos';

function QStatusTipEvent_create(tip: PWideString): QStatusTipEventH; cdecl; external QtIntf name 'QStatusTipEvent_create';
procedure QStatusTipEvent_destroy(handle: QStatusTipEventH); cdecl; external QtIntf name 'QStatusTipEvent_destroy'; 
procedure QStatusTipEvent_tip(handle: QStatusTipEventH; retval: PWideString); cdecl; external QtIntf name 'QStatusTipEvent_tip';

function QWhatsThisClickedEvent_create(href: PWideString): QWhatsThisClickedEventH; cdecl; external QtIntf name 'QWhatsThisClickedEvent_create';
procedure QWhatsThisClickedEvent_destroy(handle: QWhatsThisClickedEventH); cdecl; external QtIntf name 'QWhatsThisClickedEvent_destroy'; 
procedure QWhatsThisClickedEvent_href(handle: QWhatsThisClickedEventH; retval: PWideString); cdecl; external QtIntf name 'QWhatsThisClickedEvent_href';

function QActionEvent_create(_type: Integer; action: QActionH; before: QActionH = nil): QActionEventH; cdecl; external QtIntf name 'QActionEvent_create';
procedure QActionEvent_destroy(handle: QActionEventH); cdecl; external QtIntf name 'QActionEvent_destroy'; 
function QActionEvent_action(handle: QActionEventH): QActionH; cdecl; external QtIntf name 'QActionEvent_action';
function QActionEvent_before(handle: QActionEventH): QActionH; cdecl; external QtIntf name 'QActionEvent_before';

function QFileOpenEvent_create(_file: PWideString): QFileOpenEventH; cdecl; external QtIntf name 'QFileOpenEvent_create';
procedure QFileOpenEvent_destroy(handle: QFileOpenEventH); cdecl; external QtIntf name 'QFileOpenEvent_destroy'; 
procedure QFileOpenEvent_file(handle: QFileOpenEventH; retval: PWideString); cdecl; external QtIntf name 'QFileOpenEvent_file';

function QShortcutEvent_create(key: QKeySequenceH; id: Integer; ambiguous: Boolean = False): QShortcutEventH; cdecl; external QtIntf name 'QShortcutEvent_create';
procedure QShortcutEvent_destroy(handle: QShortcutEventH); cdecl; external QtIntf name 'QShortcutEvent_destroy'; 
function QShortcutEvent_key(handle: QShortcutEventH): QKeySequenceH; cdecl; external QtIntf name 'QShortcutEvent_key';
function QShortcutEvent_shortcutId(handle: QShortcutEventH): Integer; cdecl; external QtIntf name 'QShortcutEvent_shortcutId';
function QShortcutEvent_isAmbiguous(handle: QShortcutEventH): Boolean; cdecl; external QtIntf name 'QShortcutEvent_isAmbiguous';

function QWindowStateChangeEvent_create(aOldState: QtWindowStates): QWindowStateChangeEventH; overload; cdecl; external QtIntf name 'QWindowStateChangeEvent_create';
procedure QWindowStateChangeEvent_destroy(handle: QWindowStateChangeEventH); cdecl; external QtIntf name 'QWindowStateChangeEvent_destroy'; 
function QWindowStateChangeEvent_create(aOldState: QtWindowStates; isOverride: Boolean): QWindowStateChangeEventH; overload; cdecl; external QtIntf name 'QWindowStateChangeEvent_create2';
function QWindowStateChangeEvent_oldState(handle: QWindowStateChangeEventH): QtWindowStates; cdecl; external QtIntf name 'QWindowStateChangeEvent_oldState';
function QWindowStateChangeEvent_isOverride(handle: QWindowStateChangeEventH): Boolean; cdecl; external QtIntf name 'QWindowStateChangeEvent_isOverride';

function QLCLMessageEvent_create(aType: QEventType): QLCLMessageEventH; overload; cdecl; external QtIntf name 'QLCLMessageEvent_create';
procedure QLCLMessageEvent_destroy(handle: QLCLMessageEventH); cdecl; external QtIntf name 'QLCLMessageEvent_destroy'; 
function QLCLMessageEvent_create(aType: QEventType; aMsg: LongWord; aWParam: LongWord; aLParam: LongWord; aMsgResult: LongWord): QLCLMessageEventH; overload; cdecl; external QtIntf name 'QLCLMessageEvent_create2';
function QLCLMessageEvent_getMsg(handle: QLCLMessageEventH): LongWord; cdecl; external QtIntf name 'QLCLMessageEvent_getMsg';
function QLCLMessageEvent_getWParam(handle: QLCLMessageEventH): LongWord; cdecl; external QtIntf name 'QLCLMessageEvent_getWParam';
function QLCLMessageEvent_getLParam(handle: QLCLMessageEventH): LongWord; cdecl; external QtIntf name 'QLCLMessageEvent_getLParam';
function QLCLMessageEvent_getMsgResult(handle: QLCLMessageEventH): LongWord; cdecl; external QtIntf name 'QLCLMessageEvent_getMsgResult';
procedure QLCLMessageEvent_setMsg(handle: QLCLMessageEventH; Value: LongWord); cdecl; external QtIntf name 'QLCLMessageEvent_setMsg';
procedure QLCLMessageEvent_setWParam(handle: QLCLMessageEventH; Value: LongWord); cdecl; external QtIntf name 'QLCLMessageEvent_setWParam';
procedure QLCLMessageEvent_setLParam(handle: QLCLMessageEventH; Value: LongWord); cdecl; external QtIntf name 'QLCLMessageEvent_setLParam';
procedure QLCLMessageEvent_setMsgResult(handle: QLCLMessageEventH; Value: LongWord); cdecl; external QtIntf name 'QLCLMessageEvent_setMsgResult';

function QCursor_create(): QCursorH; overload; cdecl; external QtIntf name 'QCursor_create';
procedure QCursor_destroy(handle: QCursorH); cdecl; external QtIntf name 'QCursor_destroy'; 
function QCursor_create(shape: QtCursorShape): QCursorH; overload; cdecl; external QtIntf name 'QCursor_create2';
function QCursor_create(bitmap: QBitmapH; mask: QBitmapH; hotX: Integer = -1; hotY: Integer = -1): QCursorH; overload; cdecl; external QtIntf name 'QCursor_create3';
function QCursor_create(pixmap: QPixmapH; hotX: Integer = -1; hotY: Integer = -1): QCursorH; overload; cdecl; external QtIntf name 'QCursor_create4';
function QCursor_create(cursor: QCursorH): QCursorH; overload; cdecl; external QtIntf name 'QCursor_create5';
function QCursor_shape(handle: QCursorH): QtCursorShape; cdecl; external QtIntf name 'QCursor_shape';
procedure QCursor_setShape(handle: QCursorH; newShape: QtCursorShape); cdecl; external QtIntf name 'QCursor_setShape';
function QCursor_bitmap(handle: QCursorH): QBitmapH; cdecl; external QtIntf name 'QCursor_bitmap';
function QCursor_mask(handle: QCursorH): QBitmapH; cdecl; external QtIntf name 'QCursor_mask';
procedure QCursor_pixmap(handle: QCursorH; retval: QPixmapH); cdecl; external QtIntf name 'QCursor_pixmap';
procedure QCursor_hotSpot(handle: QCursorH; retval: PQtPoint); cdecl; external QtIntf name 'QCursor_hotSpot';
procedure QCursor_pos(retval: PQtPoint); cdecl; external QtIntf name 'QCursor_pos';
procedure QCursor_setPos(x: Integer; y: Integer); overload; cdecl; external QtIntf name 'QCursor_setPos';
procedure QCursor_setPos(p: PQtPoint); overload; cdecl; external QtIntf name 'QCursor_setPos2';
{$ifdef UNIX or DARWIN }
function QCursor_handle(handle: QCursorH): QtHANDLE; overload; cdecl; external QtIntf name 'QCursor_handle';
{$endif}
{$ifdef UNIX }
function QCursor_create(cursor: QtHANDLE): QCursorH; overload; cdecl; external QtIntf name 'QCursor_create6';
function QCursor_x11Screen(): Integer; cdecl; external QtIntf name 'QCursor_x11Screen';
{$endif}
{$ifdef MSWINDOWS }
function QCursor_handle(handle: QCursorH): HCURSOR; overload; cdecl; external QtIntf name 'QCursor_handle2';
function QCursor_create(cursor: HCURSOR): QCursorH; overload; cdecl; external QtIntf name 'QCursor_create7';
{$endif}

function QGridLayout_create(parent: QWidgetH): QGridLayoutH; overload; cdecl; external QtIntf name 'QGridLayout_create';
procedure QGridLayout_destroy(handle: QGridLayoutH); cdecl; external QtIntf name 'QGridLayout_destroy'; 
function QGridLayout_create(): QGridLayoutH; overload; cdecl; external QtIntf name 'QGridLayout_create2';
procedure QGridLayout_sizeHint(handle: QGridLayoutH; retval: PSize); cdecl; external QtIntf name 'QGridLayout_sizeHint';
procedure QGridLayout_minimumSize(handle: QGridLayoutH; retval: PSize); cdecl; external QtIntf name 'QGridLayout_minimumSize';
procedure QGridLayout_maximumSize(handle: QGridLayoutH; retval: PSize); cdecl; external QtIntf name 'QGridLayout_maximumSize';
procedure QGridLayout_setRowStretch(handle: QGridLayoutH; row: Integer; stretch: Integer); cdecl; external QtIntf name 'QGridLayout_setRowStretch';
procedure QGridLayout_setColumnStretch(handle: QGridLayoutH; column: Integer; stretch: Integer); cdecl; external QtIntf name 'QGridLayout_setColumnStretch';
function QGridLayout_rowStretch(handle: QGridLayoutH; row: Integer): Integer; cdecl; external QtIntf name 'QGridLayout_rowStretch';
function QGridLayout_columnStretch(handle: QGridLayoutH; column: Integer): Integer; cdecl; external QtIntf name 'QGridLayout_columnStretch';
procedure QGridLayout_setRowMinimumHeight(handle: QGridLayoutH; row: Integer; minSize: Integer); cdecl; external QtIntf name 'QGridLayout_setRowMinimumHeight';
procedure QGridLayout_setColumnMinimumWidth(handle: QGridLayoutH; column: Integer; minSize: Integer); cdecl; external QtIntf name 'QGridLayout_setColumnMinimumWidth';
function QGridLayout_rowMinimumHeight(handle: QGridLayoutH; row: Integer): Integer; cdecl; external QtIntf name 'QGridLayout_rowMinimumHeight';
function QGridLayout_columnMinimumWidth(handle: QGridLayoutH; column: Integer): Integer; cdecl; external QtIntf name 'QGridLayout_columnMinimumWidth';
function QGridLayout_columnCount(handle: QGridLayoutH): Integer; cdecl; external QtIntf name 'QGridLayout_columnCount';
function QGridLayout_rowCount(handle: QGridLayoutH): Integer; cdecl; external QtIntf name 'QGridLayout_rowCount';
procedure QGridLayout_cellRect(handle: QGridLayoutH; retval: PRect; row: Integer; column: Integer); cdecl; external QtIntf name 'QGridLayout_cellRect';
function QGridLayout_hasHeightForWidth(handle: QGridLayoutH): Boolean; cdecl; external QtIntf name 'QGridLayout_hasHeightForWidth';
function QGridLayout_heightForWidth(handle: QGridLayoutH; p1: Integer): Integer; cdecl; external QtIntf name 'QGridLayout_heightForWidth';
function QGridLayout_minimumHeightForWidth(handle: QGridLayoutH; p1: Integer): Integer; cdecl; external QtIntf name 'QGridLayout_minimumHeightForWidth';
function QGridLayout_expandingDirections(handle: QGridLayoutH): QtOrientations; cdecl; external QtIntf name 'QGridLayout_expandingDirections';
procedure QGridLayout_invalidate(handle: QGridLayoutH); cdecl; external QtIntf name 'QGridLayout_invalidate';
procedure QGridLayout_addWidget(handle: QGridLayoutH; w: QWidgetH); overload; cdecl; external QtIntf name 'QGridLayout_addWidget';
procedure QGridLayout_addWidget(handle: QGridLayoutH; p1: QWidgetH; row: Integer; column: Integer; p4: QtAlignment = 0); overload; cdecl; external QtIntf name 'QGridLayout_addWidget2';
procedure QGridLayout_addWidget(handle: QGridLayoutH; p1: QWidgetH; row: Integer; column: Integer; rowSpan: Integer; columnSpan: Integer; p6: QtAlignment = 0); overload; cdecl; external QtIntf name 'QGridLayout_addWidget3';
procedure QGridLayout_addLayout(handle: QGridLayoutH; p1: QLayoutH; row: Integer; column: Integer; p4: QtAlignment = 0); overload; cdecl; external QtIntf name 'QGridLayout_addLayout';
procedure QGridLayout_addLayout(handle: QGridLayoutH; p1: QLayoutH; row: Integer; column: Integer; rowSpan: Integer; columnSpan: Integer; p6: QtAlignment = 0); overload; cdecl; external QtIntf name 'QGridLayout_addLayout2';
procedure QGridLayout_setOriginCorner(handle: QGridLayoutH; p1: QtCorner); cdecl; external QtIntf name 'QGridLayout_setOriginCorner';
function QGridLayout_originCorner(handle: QGridLayoutH): QtCorner; cdecl; external QtIntf name 'QGridLayout_originCorner';
function QGridLayout_itemAt(handle: QGridLayoutH; p1: Integer): QLayoutItemH; cdecl; external QtIntf name 'QGridLayout_itemAt';
function QGridLayout_takeAt(handle: QGridLayoutH; p1: Integer): QLayoutItemH; cdecl; external QtIntf name 'QGridLayout_takeAt';
function QGridLayout_count(handle: QGridLayoutH): Integer; cdecl; external QtIntf name 'QGridLayout_count';
procedure QGridLayout_setGeometry(handle: QGridLayoutH; p1: PRect); cdecl; external QtIntf name 'QGridLayout_setGeometry';
procedure QGridLayout_addItem(handle: QGridLayoutH; item: QLayoutItemH; row: Integer; column: Integer; rowSpan: Integer = 1; columnSpan: Integer = 1; p6: QtAlignment = 0); cdecl; external QtIntf name 'QGridLayout_addItem';
procedure QGridLayout_setDefaultPositioning(handle: QGridLayoutH; n: Integer; orient: QtOrientation); cdecl; external QtIntf name 'QGridLayout_setDefaultPositioning';
procedure QGridLayout_getItemPosition(handle: QGridLayoutH; idx: Integer; row: PInteger; column: PInteger; rowSpan: PInteger; columnSpan: PInteger); cdecl; external QtIntf name 'QGridLayout_getItemPosition';

type
  QClipboardMode = cardinal; //  QClipboard::Mode (4)

const
    QClipboardClipboard = 0 { $0 };
    QClipboardSelection = 1 { $1 };
    QClipboardFindBuffer = 2 { $2 };
    QClipboardLastMode = 2 { $2 };


procedure QClipboard_clear(handle: QClipboardH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtIntf name 'QClipboard_clear';
function QClipboard_supportsSelection(handle: QClipboardH): Boolean; cdecl; external QtIntf name 'QClipboard_supportsSelection';
function QClipboard_supportsFindBuffer(handle: QClipboardH): Boolean; cdecl; external QtIntf name 'QClipboard_supportsFindBuffer';
function QClipboard_ownsSelection(handle: QClipboardH): Boolean; cdecl; external QtIntf name 'QClipboard_ownsSelection';
function QClipboard_ownsClipboard(handle: QClipboardH): Boolean; cdecl; external QtIntf name 'QClipboard_ownsClipboard';
function QClipboard_ownsFindBuffer(handle: QClipboardH): Boolean; cdecl; external QtIntf name 'QClipboard_ownsFindBuffer';
procedure QClipboard_text(handle: QClipboardH; retval: PWideString; mode: QClipboardMode = QClipboardClipboard); overload; cdecl; external QtIntf name 'QClipboard_text';
procedure QClipboard_text(handle: QClipboardH; retval: PWideString; subtype: PWideString; mode: QClipboardMode = QClipboardClipboard); overload; cdecl; external QtIntf name 'QClipboard_text2';
procedure QClipboard_setText(handle: QClipboardH; p1: PWideString; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtIntf name 'QClipboard_setText';
function QClipboard_mimeData(handle: QClipboardH; mode: QClipboardMode = QClipboardClipboard): QMimeDataH; cdecl; external QtIntf name 'QClipboard_mimeData';
procedure QClipboard_setMimeData(handle: QClipboardH; data: QMimeDataH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtIntf name 'QClipboard_setMimeData';
procedure QClipboard_image(handle: QClipboardH; retval: QImageH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtIntf name 'QClipboard_image';
procedure QClipboard_pixmap(handle: QClipboardH; retval: QPixmapH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtIntf name 'QClipboard_pixmap';
procedure QClipboard_setImage(handle: QClipboardH; p1: QImageH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtIntf name 'QClipboard_setImage';
procedure QClipboard_setPixmap(handle: QClipboardH; p1: QPixmapH; mode: QClipboardMode = QClipboardClipboard); cdecl; external QtIntf name 'QClipboard_setPixmap';


type
  QClipboard_changed_Event = procedure (mode: QClipboardMode) of object cdecl;
  QClipboard_selectionChanged_Event = procedure () of object cdecl;
  QClipboard_findBufferChanged_Event = procedure () of object cdecl;
  QClipboard_dataChanged_Event = procedure () of object cdecl;


function QDesktopWidget_create(): QDesktopWidgetH; cdecl; external QtIntf name 'QDesktopWidget_create';
procedure QDesktopWidget_destroy(handle: QDesktopWidgetH); cdecl; external QtIntf name 'QDesktopWidget_destroy'; 
function QDesktopWidget_isVirtualDesktop(handle: QDesktopWidgetH): Boolean; cdecl; external QtIntf name 'QDesktopWidget_isVirtualDesktop';
function QDesktopWidget_numScreens(handle: QDesktopWidgetH): Integer; cdecl; external QtIntf name 'QDesktopWidget_numScreens';
function QDesktopWidget_primaryScreen(handle: QDesktopWidgetH): Integer; cdecl; external QtIntf name 'QDesktopWidget_primaryScreen';
function QDesktopWidget_screenNumber(handle: QDesktopWidgetH; widget: QWidgetH = nil): Integer; overload; cdecl; external QtIntf name 'QDesktopWidget_screenNumber';
function QDesktopWidget_screenNumber(handle: QDesktopWidgetH; p1: PQtPoint): Integer; overload; cdecl; external QtIntf name 'QDesktopWidget_screenNumber2';
function QDesktopWidget_screen(handle: QDesktopWidgetH; screen: Integer = -1): QWidgetH; cdecl; external QtIntf name 'QDesktopWidget_screen';
procedure QDesktopWidget_screenGeometry(handle: QDesktopWidgetH; retval: PRect; screen: Integer = -1); overload; cdecl; external QtIntf name 'QDesktopWidget_screenGeometry';
procedure QDesktopWidget_screenGeometry(handle: QDesktopWidgetH; retval: PRect; widget: QWidgetH); overload; cdecl; external QtIntf name 'QDesktopWidget_screenGeometry2';
procedure QDesktopWidget_screenGeometry(handle: QDesktopWidgetH; retval: PRect; point: PQtPoint); overload; cdecl; external QtIntf name 'QDesktopWidget_screenGeometry3';
procedure QDesktopWidget_availableGeometry(handle: QDesktopWidgetH; retval: PRect; screen: Integer = -1); overload; cdecl; external QtIntf name 'QDesktopWidget_availableGeometry';
procedure QDesktopWidget_availableGeometry(handle: QDesktopWidgetH; retval: PRect; widget: QWidgetH); overload; cdecl; external QtIntf name 'QDesktopWidget_availableGeometry2';
procedure QDesktopWidget_availableGeometry(handle: QDesktopWidgetH; retval: PRect; point: PQtPoint); overload; cdecl; external QtIntf name 'QDesktopWidget_availableGeometry3';


type
  QDesktopWidget_resized_Event = procedure (p1: Integer) of object cdecl;
  QDesktopWidget_workAreaResized_Event = procedure (p1: Integer) of object cdecl;


procedure QToolTip_showText(pos: PQtPoint; text: PWideString; w: QWidgetH = nil); overload; cdecl; external QtIntf name 'QToolTip_showText';
procedure QToolTip_showText(pos: PQtPoint; text: PWideString; w: QWidgetH; rect: PRect); overload; cdecl; external QtIntf name 'QToolTip_showText2';
procedure QToolTip_hideText(); cdecl; external QtIntf name 'QToolTip_hideText';
procedure QToolTip_palette(retval: QPaletteH); cdecl; external QtIntf name 'QToolTip_palette';
procedure QToolTip_setPalette(p1: QPaletteH); cdecl; external QtIntf name 'QToolTip_setPalette';
procedure QToolTip_font(retval: QFontH); cdecl; external QtIntf name 'QToolTip_font';
procedure QToolTip_setFont(p1: QFontH); cdecl; external QtIntf name 'QToolTip_setFont';

{$ifdef UNIX }
function QX11Info_create(): QX11InfoH; overload; cdecl; external QtIntf name 'QX11Info_create';
procedure QX11Info_destroy(handle: QX11InfoH); cdecl; external QtIntf name 'QX11Info_destroy'; 
function QX11Info_create(other: QX11InfoH): QX11InfoH; overload; cdecl; external QtIntf name 'QX11Info_create2';
function QX11Info_display(): PDisplay; cdecl; external QtIntf name 'QX11Info_display';
function QX11Info_appClass(): PAnsiChar; cdecl; external QtIntf name 'QX11Info_appClass';
function QX11Info_screen(handle: QX11InfoH): Integer; cdecl; external QtIntf name 'QX11Info_screen';
function QX11Info_depth(handle: QX11InfoH): Integer; cdecl; external QtIntf name 'QX11Info_depth';
function QX11Info_cells(handle: QX11InfoH): Integer; cdecl; external QtIntf name 'QX11Info_cells';
function QX11Info_colormap(handle: QX11InfoH): QtHANDLE; cdecl; external QtIntf name 'QX11Info_colormap';
function QX11Info_defaultColormap(handle: QX11InfoH): Boolean; cdecl; external QtIntf name 'QX11Info_defaultColormap';
function QX11Info_visual(handle: QX11InfoH): Pointer; cdecl; external QtIntf name 'QX11Info_visual';
function QX11Info_defaultVisual(handle: QX11InfoH): Boolean; cdecl; external QtIntf name 'QX11Info_defaultVisual';
function QX11Info_appScreen(): Integer; cdecl; external QtIntf name 'QX11Info_appScreen';
function QX11Info_appDepth(screen: Integer = -1): Integer; cdecl; external QtIntf name 'QX11Info_appDepth';
function QX11Info_appCells(screen: Integer = -1): Integer; cdecl; external QtIntf name 'QX11Info_appCells';
function QX11Info_appColormap(screen: Integer = -1): QtHANDLE; cdecl; external QtIntf name 'QX11Info_appColormap';
function QX11Info_appVisual(screen: Integer = -1): Pointer; cdecl; external QtIntf name 'QX11Info_appVisual';
function QX11Info_appRootWindow(screen: Integer = -1): QtHANDLE; cdecl; external QtIntf name 'QX11Info_appRootWindow';
function QX11Info_appDefaultColormap(screen: Integer = -1): Boolean; cdecl; external QtIntf name 'QX11Info_appDefaultColormap';
function QX11Info_appDefaultVisual(screen: Integer = -1): Boolean; cdecl; external QtIntf name 'QX11Info_appDefaultVisual';
function QX11Info_appDpiX(screen: Integer = -1): Integer; cdecl; external QtIntf name 'QX11Info_appDpiX';
function QX11Info_appDpiY(screen: Integer = -1): Integer; cdecl; external QtIntf name 'QX11Info_appDpiY';
procedure QX11Info_setAppDpiX(screen: Integer; dpi: Integer); cdecl; external QtIntf name 'QX11Info_setAppDpiX';
procedure QX11Info_setAppDpiY(screen: Integer; dpi: Integer); cdecl; external QtIntf name 'QX11Info_setAppDpiY';
function QX11Info_appTime(): Longword; cdecl; external QtIntf name 'QX11Info_appTime';
function QX11Info_appUserTime(): Longword; cdecl; external QtIntf name 'QX11Info_appUserTime';
procedure QX11Info_setAppTime(time: Longword); cdecl; external QtIntf name 'QX11Info_setAppTime';
procedure QX11Info_setAppUserTime(time: Longword); cdecl; external QtIntf name 'QX11Info_setAppUserTime';
{$endif}

function QMimeSource_format(handle: QMimeSourceH; n: Integer = 0): PAnsiChar; cdecl; external QtIntf name 'QMimeSource_format';
function QMimeSource_provides(handle: QMimeSourceH; p1: PAnsiChar): Boolean; cdecl; external QtIntf name 'QMimeSource_provides';
procedure QMimeSource_encodedData(handle: QMimeSourceH; retval: QByteArrayH; p1: PAnsiChar); cdecl; external QtIntf name 'QMimeSource_encodedData';

function QDrag_create(dragSource: QWidgetH): QDragH; cdecl; external QtIntf name 'QDrag_create';
procedure QDrag_destroy(handle: QDragH); cdecl; external QtIntf name 'QDrag_destroy'; 
procedure QDrag_setMimeData(handle: QDragH; data: QMimeDataH); cdecl; external QtIntf name 'QDrag_setMimeData';
function QDrag_mimeData(handle: QDragH): QMimeDataH; cdecl; external QtIntf name 'QDrag_mimeData';
procedure QDrag_setPixmap(handle: QDragH; p1: QPixmapH); cdecl; external QtIntf name 'QDrag_setPixmap';
procedure QDrag_pixmap(handle: QDragH; retval: QPixmapH); cdecl; external QtIntf name 'QDrag_pixmap';
procedure QDrag_setHotSpot(handle: QDragH; hotspot: PQtPoint); cdecl; external QtIntf name 'QDrag_setHotSpot';
procedure QDrag_hotSpot(handle: QDragH; retval: PQtPoint); cdecl; external QtIntf name 'QDrag_hotSpot';
function QDrag_source(handle: QDragH): QWidgetH; cdecl; external QtIntf name 'QDrag_source';
function QDrag_target(handle: QDragH): QWidgetH; cdecl; external QtIntf name 'QDrag_target';
function QDrag_start(handle: QDragH; supportedActions: QtDropActions = QtCopyAction): QtDropAction; cdecl; external QtIntf name 'QDrag_start';
procedure QDrag_setDragCursor(handle: QDragH; cursor: QPixmapH; action: QtDropAction); cdecl; external QtIntf name 'QDrag_setDragCursor';


type
  QDrag_actionChanged_Event = procedure (action: QtDropAction) of object cdecl;
  QDrag_targetChanged_Event = procedure (newTarget: QWidgetH) of object cdecl;



type
  QColorSpec = ( // QColor::Spec (1)
    QColorInvalid, QColorRgb, QColorHsv, QColorCmyk );

function QColor_create(): QColorH; overload; cdecl; external QtIntf name 'QColor_create';
procedure QColor_destroy(handle: QColorH); cdecl; external QtIntf name 'QColor_destroy'; 
function QColor_create(color: QtGlobalColor): QColorH; overload; cdecl; external QtIntf name 'QColor_create2';
function QColor_create(r: Integer; g: Integer; b: Integer; a: Integer = 255): QColorH; overload; cdecl; external QtIntf name 'QColor_create3';
function QColor_create(rgb: QRgb): QColorH; overload; cdecl; external QtIntf name 'QColor_create4';
function QColor_create(name: PWideString): QColorH; overload; cdecl; external QtIntf name 'QColor_create5';
function QColor_create(name: PAnsiChar): QColorH; overload; cdecl; external QtIntf name 'QColor_create6';
function QColor_create(color: PQColor): QColorH; overload; cdecl; external QtIntf name 'QColor_create7';
function QColor_create(spec: QColorSpec): QColorH; overload; cdecl; external QtIntf name 'QColor_create8';
function QColor_isValid(handle: QColorH): Boolean; cdecl; external QtIntf name 'QColor_isValid';
procedure QColor_name(handle: QColorH; retval: PWideString); cdecl; external QtIntf name 'QColor_name';
procedure QColor_setNamedColor(handle: QColorH; name: PWideString); cdecl; external QtIntf name 'QColor_setNamedColor';
procedure QColor_colorNames(retval: QStringListH); cdecl; external QtIntf name 'QColor_colorNames';
function QColor_spec(handle: QColorH): QColorSpec; cdecl; external QtIntf name 'QColor_spec';
function QColor_alpha(handle: QColorH): Integer; cdecl; external QtIntf name 'QColor_alpha';
procedure QColor_setAlpha(handle: QColorH; alpha: Integer); cdecl; external QtIntf name 'QColor_setAlpha';
function QColor_alphaF(handle: QColorH): Double; cdecl; external QtIntf name 'QColor_alphaF';
procedure QColor_setAlphaF(handle: QColorH; alpha: Double); cdecl; external QtIntf name 'QColor_setAlphaF';
function QColor_red(handle: QColorH): Integer; cdecl; external QtIntf name 'QColor_red';
function QColor_green(handle: QColorH): Integer; cdecl; external QtIntf name 'QColor_green';
function QColor_blue(handle: QColorH): Integer; cdecl; external QtIntf name 'QColor_blue';
procedure QColor_setRed(handle: QColorH; red: Integer); cdecl; external QtIntf name 'QColor_setRed';
procedure QColor_setGreen(handle: QColorH; green: Integer); cdecl; external QtIntf name 'QColor_setGreen';
procedure QColor_setBlue(handle: QColorH; blue: Integer); cdecl; external QtIntf name 'QColor_setBlue';
function QColor_redF(handle: QColorH): Double; cdecl; external QtIntf name 'QColor_redF';
function QColor_greenF(handle: QColorH): Double; cdecl; external QtIntf name 'QColor_greenF';
function QColor_blueF(handle: QColorH): Double; cdecl; external QtIntf name 'QColor_blueF';
procedure QColor_setRedF(handle: QColorH; red: Double); cdecl; external QtIntf name 'QColor_setRedF';
procedure QColor_setGreenF(handle: QColorH; green: Double); cdecl; external QtIntf name 'QColor_setGreenF';
procedure QColor_setBlueF(handle: QColorH; blue: Double); cdecl; external QtIntf name 'QColor_setBlueF';
procedure QColor_getRgb(handle: QColorH; r: PInteger; g: PInteger; b: PInteger; a: PInteger = nil); cdecl; external QtIntf name 'QColor_getRgb';
procedure QColor_setRgb(handle: QColorH; r: Integer; g: Integer; b: Integer; a: Integer = 255); overload; cdecl; external QtIntf name 'QColor_setRgb';
procedure QColor_getRgbF(handle: QColorH; r: PDouble; g: PDouble; b: PDouble; a: PDouble = nil); cdecl; external QtIntf name 'QColor_getRgbF';
procedure QColor_setRgbF(handle: QColorH; r: Double; g: Double; b: Double; a: Double = 1.0); cdecl; external QtIntf name 'QColor_setRgbF';
function QColor_rgba(handle: QColorH): QRgb; cdecl; external QtIntf name 'QColor_rgba';
procedure QColor_setRgba(handle: QColorH; rgba: QRgb); cdecl; external QtIntf name 'QColor_setRgba';
function QColor_rgb(handle: QColorH): QRgb; cdecl; external QtIntf name 'QColor_rgb';
procedure QColor_setRgb(handle: QColorH; rgb: QRgb); overload; cdecl; external QtIntf name 'QColor_setRgb2';
function QColor_hue(handle: QColorH): Integer; cdecl; external QtIntf name 'QColor_hue';
function QColor_saturation(handle: QColorH): Integer; cdecl; external QtIntf name 'QColor_saturation';
function QColor_value(handle: QColorH): Integer; cdecl; external QtIntf name 'QColor_value';
function QColor_hueF(handle: QColorH): Double; cdecl; external QtIntf name 'QColor_hueF';
function QColor_saturationF(handle: QColorH): Double; cdecl; external QtIntf name 'QColor_saturationF';
function QColor_valueF(handle: QColorH): Double; cdecl; external QtIntf name 'QColor_valueF';
procedure QColor_getHsv(handle: QColorH; h: PInteger; s: PInteger; v: PInteger; a: PInteger = nil); cdecl; external QtIntf name 'QColor_getHsv';
procedure QColor_setHsv(handle: QColorH; h: Integer; s: Integer; v: Integer; a: Integer = 255); cdecl; external QtIntf name 'QColor_setHsv';
procedure QColor_getHsvF(handle: QColorH; h: PDouble; s: PDouble; v: PDouble; a: PDouble = nil); cdecl; external QtIntf name 'QColor_getHsvF';
procedure QColor_setHsvF(handle: QColorH; h: Double; s: Double; v: Double; a: Double = 1.0); cdecl; external QtIntf name 'QColor_setHsvF';
function QColor_cyan(handle: QColorH): Integer; cdecl; external QtIntf name 'QColor_cyan';
function QColor_magenta(handle: QColorH): Integer; cdecl; external QtIntf name 'QColor_magenta';
function QColor_yellow(handle: QColorH): Integer; cdecl; external QtIntf name 'QColor_yellow';
function QColor_black(handle: QColorH): Integer; cdecl; external QtIntf name 'QColor_black';
function QColor_cyanF(handle: QColorH): Double; cdecl; external QtIntf name 'QColor_cyanF';
function QColor_magentaF(handle: QColorH): Double; cdecl; external QtIntf name 'QColor_magentaF';
function QColor_yellowF(handle: QColorH): Double; cdecl; external QtIntf name 'QColor_yellowF';
function QColor_blackF(handle: QColorH): Double; cdecl; external QtIntf name 'QColor_blackF';
procedure QColor_getCmyk(handle: QColorH; c: PInteger; m: PInteger; y: PInteger; k: PInteger; a: PInteger = nil); cdecl; external QtIntf name 'QColor_getCmyk';
procedure QColor_setCmyk(handle: QColorH; c: Integer; m: Integer; y: Integer; k: Integer; a: Integer = 255); cdecl; external QtIntf name 'QColor_setCmyk';
procedure QColor_getCmykF(handle: QColorH; c: PDouble; m: PDouble; y: PDouble; k: PDouble; a: PDouble = nil); cdecl; external QtIntf name 'QColor_getCmykF';
procedure QColor_setCmykF(handle: QColorH; c: Double; m: Double; y: Double; k: Double; a: Double = 1.0); cdecl; external QtIntf name 'QColor_setCmykF';
procedure QColor_toRgb(handle: QColorH; retval: PQColor); cdecl; external QtIntf name 'QColor_toRgb';
procedure QColor_toHsv(handle: QColorH; retval: PQColor); cdecl; external QtIntf name 'QColor_toHsv';
procedure QColor_toCmyk(handle: QColorH; retval: PQColor); cdecl; external QtIntf name 'QColor_toCmyk';
procedure QColor_convertTo(handle: QColorH; retval: PQColor; colorSpec: QColorSpec); cdecl; external QtIntf name 'QColor_convertTo';
procedure QColor_fromRgb(retval: PQColor; rgb: QRgb); overload; cdecl; external QtIntf name 'QColor_fromRgb';
procedure QColor_fromRgba(retval: PQColor; rgba: QRgb); cdecl; external QtIntf name 'QColor_fromRgba';
procedure QColor_fromRgb(retval: PQColor; r: Integer; g: Integer; b: Integer; a: Integer = 255); overload; cdecl; external QtIntf name 'QColor_fromRgb2';
procedure QColor_fromRgbF(retval: PQColor; r: Double; g: Double; b: Double; a: Double = 1.0); cdecl; external QtIntf name 'QColor_fromRgbF';
procedure QColor_fromHsv(retval: PQColor; h: Integer; s: Integer; v: Integer; a: Integer = 255); cdecl; external QtIntf name 'QColor_fromHsv';
procedure QColor_fromHsvF(retval: PQColor; h: Double; s: Double; v: Double; a: Double = 1.0); cdecl; external QtIntf name 'QColor_fromHsvF';
procedure QColor_fromCmyk(retval: PQColor; c: Integer; m: Integer; y: Integer; k: Integer; a: Integer = 255); cdecl; external QtIntf name 'QColor_fromCmyk';
procedure QColor_fromCmykF(retval: PQColor; c: Double; m: Double; y: Double; k: Double; a: Double = 1.0); cdecl; external QtIntf name 'QColor_fromCmykF';
procedure QColor_light(handle: QColorH; retval: PQColor; f: Integer = 150); cdecl; external QtIntf name 'QColor_light';
procedure QColor_dark(handle: QColorH; retval: PQColor; f: Integer = 200); cdecl; external QtIntf name 'QColor_dark';

function QMatrix_create(): QMatrixH; overload; cdecl; external QtIntf name 'QMatrix_create';
procedure QMatrix_destroy(handle: QMatrixH); cdecl; external QtIntf name 'QMatrix_destroy'; 
function QMatrix_create(m11: Double; m12: Double; m21: Double; m22: Double; dx: Double; dy: Double): QMatrixH; overload; cdecl; external QtIntf name 'QMatrix_create2';
function QMatrix_create(matrix: QMatrixH): QMatrixH; overload; cdecl; external QtIntf name 'QMatrix_create3';
procedure QMatrix_setMatrix(handle: QMatrixH; m11: Double; m12: Double; m21: Double; m22: Double; dx: Double; dy: Double); cdecl; external QtIntf name 'QMatrix_setMatrix';
function QMatrix_m11(handle: QMatrixH): Double; cdecl; external QtIntf name 'QMatrix_m11';
function QMatrix_m12(handle: QMatrixH): Double; cdecl; external QtIntf name 'QMatrix_m12';
function QMatrix_m21(handle: QMatrixH): Double; cdecl; external QtIntf name 'QMatrix_m21';
function QMatrix_m22(handle: QMatrixH): Double; cdecl; external QtIntf name 'QMatrix_m22';
function QMatrix_dx(handle: QMatrixH): Double; cdecl; external QtIntf name 'QMatrix_dx';
function QMatrix_dy(handle: QMatrixH): Double; cdecl; external QtIntf name 'QMatrix_dy';
procedure QMatrix_map(handle: QMatrixH; x: Integer; y: Integer; tx: PInteger; ty: PInteger); overload; cdecl; external QtIntf name 'QMatrix_map';
procedure QMatrix_map(handle: QMatrixH; x: Double; y: Double; tx: PDouble; ty: PDouble); overload; cdecl; external QtIntf name 'QMatrix_map2';
procedure QMatrix_mapRect(handle: QMatrixH; retval: PRect; p1: PRect); overload; cdecl; external QtIntf name 'QMatrix_mapRect';
procedure QMatrix_mapRect(handle: QMatrixH; retval: QRectFH; p1: QRectFH); overload; cdecl; external QtIntf name 'QMatrix_mapRect2';
procedure QMatrix_map(handle: QMatrixH; retval: PQtPoint; p: PQtPoint); overload; cdecl; external QtIntf name 'QMatrix_map3';
procedure QMatrix_map(handle: QMatrixH; retval: QPointFH; p: QPointFH); overload; cdecl; external QtIntf name 'QMatrix_map4';
procedure QMatrix_map(handle: QMatrixH; retval: QLineH; l: QLineH); overload; cdecl; external QtIntf name 'QMatrix_map5';
procedure QMatrix_map(handle: QMatrixH; retval: QLineFH; l: QLineFH); overload; cdecl; external QtIntf name 'QMatrix_map6';
procedure QMatrix_map(handle: QMatrixH; retval: QPolygonFH; a: QPolygonFH); overload; cdecl; external QtIntf name 'QMatrix_map7';
procedure QMatrix_map(handle: QMatrixH; retval: QPolygonH; a: QPolygonH); overload; cdecl; external QtIntf name 'QMatrix_map8';
procedure QMatrix_map(handle: QMatrixH; retval: QRegionH; r: QRegionH); overload; cdecl; external QtIntf name 'QMatrix_map9';
procedure QMatrix_map(handle: QMatrixH; retval: QPainterPathH; p: QPainterPathH); overload; cdecl; external QtIntf name 'QMatrix_map10';
procedure QMatrix_mapToPolygon(handle: QMatrixH; retval: QPolygonH; r: PRect); cdecl; external QtIntf name 'QMatrix_mapToPolygon';
procedure QMatrix_reset(handle: QMatrixH); cdecl; external QtIntf name 'QMatrix_reset';
function QMatrix_isIdentity(handle: QMatrixH): Boolean; cdecl; external QtIntf name 'QMatrix_isIdentity';
function QMatrix_translate(handle: QMatrixH; dx: Double; dy: Double): QMatrixH; cdecl; external QtIntf name 'QMatrix_translate';
function QMatrix_scale(handle: QMatrixH; sx: Double; sy: Double): QMatrixH; cdecl; external QtIntf name 'QMatrix_scale';
function QMatrix_shear(handle: QMatrixH; sh: Double; sv: Double): QMatrixH; cdecl; external QtIntf name 'QMatrix_shear';
function QMatrix_rotate(handle: QMatrixH; a: Double): QMatrixH; cdecl; external QtIntf name 'QMatrix_rotate';
function QMatrix_isInvertible(handle: QMatrixH): Boolean; cdecl; external QtIntf name 'QMatrix_isInvertible';
function QMatrix_det(handle: QMatrixH): Double; cdecl; external QtIntf name 'QMatrix_det';
procedure QMatrix_inverted(handle: QMatrixH; retval: QMatrixH; invertible: PBoolean = nil); cdecl; external QtIntf name 'QMatrix_inverted';


type
  QGradientType = ( // QGradient::Type (1)
    QGradientLinearGradient, QGradientRadialGradient, QGradientConicalGradient, QGradientNoGradient );

  QGradientSpread = ( // QGradient::Spread (1)
    QGradientPadSpread, QGradientReflectSpread, QGradientRepeatSpread );

  QGradientCoordinateMode = ( // QGradient::CoordinateMode (1)
    QGradientLogicalMode, QGradientStretchToDeviceMode );

function QBrush_create(): QBrushH; overload; cdecl; external QtIntf name 'QBrush_create';
procedure QBrush_destroy(handle: QBrushH); cdecl; external QtIntf name 'QBrush_destroy'; 
function QBrush_create(bs: QtBrushStyle): QBrushH; overload; cdecl; external QtIntf name 'QBrush_create2';
function QBrush_create(color: PQColor; bs: QtBrushStyle = QtSolidPattern): QBrushH; overload; cdecl; external QtIntf name 'QBrush_create3';
function QBrush_create(color: QtGlobalColor; bs: QtBrushStyle = QtSolidPattern): QBrushH; overload; cdecl; external QtIntf name 'QBrush_create4';
function QBrush_create(color: PQColor; pixmap: QPixmapH): QBrushH; overload; cdecl; external QtIntf name 'QBrush_create5';
function QBrush_create(color: QtGlobalColor; pixmap: QPixmapH): QBrushH; overload; cdecl; external QtIntf name 'QBrush_create6';
function QBrush_create(pixmap: QPixmapH): QBrushH; overload; cdecl; external QtIntf name 'QBrush_create7';
function QBrush_create(image: QImageH): QBrushH; overload; cdecl; external QtIntf name 'QBrush_create8';
function QBrush_create(brush: QBrushH): QBrushH; overload; cdecl; external QtIntf name 'QBrush_create9';
function QBrush_create(gradient: QGradientH): QBrushH; overload; cdecl; external QtIntf name 'QBrush_create10';
function QBrush_style(handle: QBrushH): QtBrushStyle; cdecl; external QtIntf name 'QBrush_style';
procedure QBrush_setStyle(handle: QBrushH; p1: QtBrushStyle); cdecl; external QtIntf name 'QBrush_setStyle';
function QBrush_matrix(handle: QBrushH): QMatrixH; cdecl; external QtIntf name 'QBrush_matrix';
procedure QBrush_setMatrix(handle: QBrushH; mat: QMatrixH); cdecl; external QtIntf name 'QBrush_setMatrix';
procedure QBrush_texture(handle: QBrushH; retval: QPixmapH); cdecl; external QtIntf name 'QBrush_texture';
procedure QBrush_setTexture(handle: QBrushH; pixmap: QPixmapH); cdecl; external QtIntf name 'QBrush_setTexture';
procedure QBrush_textureImage(handle: QBrushH; retval: QImageH); cdecl; external QtIntf name 'QBrush_textureImage';
procedure QBrush_setTextureImage(handle: QBrushH; image: QImageH); cdecl; external QtIntf name 'QBrush_setTextureImage';
function QBrush_color(handle: QBrushH): PQColor; cdecl; external QtIntf name 'QBrush_color';
procedure QBrush_setColor(handle: QBrushH; color: PQColor); overload; cdecl; external QtIntf name 'QBrush_setColor';
procedure QBrush_setColor(handle: QBrushH; color: QtGlobalColor); overload; cdecl; external QtIntf name 'QBrush_setColor2';
function QBrush_gradient(handle: QBrushH): QGradientH; cdecl; external QtIntf name 'QBrush_gradient';
function QBrush_isOpaque(handle: QBrushH): Boolean; cdecl; external QtIntf name 'QBrush_isOpaque';

function QGradient_create(): QGradientH; cdecl; external QtIntf name 'QGradient_create';
procedure QGradient_destroy(handle: QGradientH); cdecl; external QtIntf name 'QGradient_destroy'; 
function QGradient_type(handle: QGradientH): QGradientType; cdecl; external QtIntf name 'QGradient_type';
procedure QGradient_setSpread(handle: QGradientH; spread: QGradientSpread); cdecl; external QtIntf name 'QGradient_setSpread';
function QGradient_spread(handle: QGradientH): QGradientSpread; cdecl; external QtIntf name 'QGradient_spread';
procedure QGradient_setColorAt(handle: QGradientH; pos: Double; color: PQColor); cdecl; external QtIntf name 'QGradient_setColorAt';
function QGradient_coordinateMode(handle: QGradientH): QGradientCoordinateMode; cdecl; external QtIntf name 'QGradient_coordinateMode';
procedure QGradient_setCoordinateMode(handle: QGradientH; mode: QGradientCoordinateMode); cdecl; external QtIntf name 'QGradient_setCoordinateMode';

function QLinearGradient_create(): QLinearGradientH; overload; cdecl; external QtIntf name 'QLinearGradient_create';
procedure QLinearGradient_destroy(handle: QLinearGradientH); cdecl; external QtIntf name 'QLinearGradient_destroy'; 
function QLinearGradient_create(start: QPointFH; finalStop: QPointFH): QLinearGradientH; overload; cdecl; external QtIntf name 'QLinearGradient_create2';
function QLinearGradient_create(xStart: Double; yStart: Double; xFinalStop: Double; yFinalStop: Double): QLinearGradientH; overload; cdecl; external QtIntf name 'QLinearGradient_create3';
procedure QLinearGradient_start(handle: QLinearGradientH; retval: QPointFH); cdecl; external QtIntf name 'QLinearGradient_start';
procedure QLinearGradient_setStart(handle: QLinearGradientH; start: QPointFH); overload; cdecl; external QtIntf name 'QLinearGradient_setStart';
procedure QLinearGradient_setStart(handle: QLinearGradientH; x: Double; y: Double); overload; cdecl; external QtIntf name 'QLinearGradient_setStart2';
procedure QLinearGradient_finalStop(handle: QLinearGradientH; retval: QPointFH); cdecl; external QtIntf name 'QLinearGradient_finalStop';
procedure QLinearGradient_setFinalStop(handle: QLinearGradientH; stop: QPointFH); overload; cdecl; external QtIntf name 'QLinearGradient_setFinalStop';
procedure QLinearGradient_setFinalStop(handle: QLinearGradientH; x: Double; y: Double); overload; cdecl; external QtIntf name 'QLinearGradient_setFinalStop2';

function QRadialGradient_create(): QRadialGradientH; overload; cdecl; external QtIntf name 'QRadialGradient_create';
procedure QRadialGradient_destroy(handle: QRadialGradientH); cdecl; external QtIntf name 'QRadialGradient_destroy'; 
function QRadialGradient_create(center: QPointFH; radius: Double; focalPoint: QPointFH): QRadialGradientH; overload; cdecl; external QtIntf name 'QRadialGradient_create2';
function QRadialGradient_create(cx: Double; cy: Double; radius: Double; fx: Double; fy: Double): QRadialGradientH; overload; cdecl; external QtIntf name 'QRadialGradient_create3';
function QRadialGradient_create(center: QPointFH; radius: Double): QRadialGradientH; overload; cdecl; external QtIntf name 'QRadialGradient_create4';
function QRadialGradient_create(cx: Double; cy: Double; radius: Double): QRadialGradientH; overload; cdecl; external QtIntf name 'QRadialGradient_create5';
procedure QRadialGradient_center(handle: QRadialGradientH; retval: QPointFH); cdecl; external QtIntf name 'QRadialGradient_center';
procedure QRadialGradient_setCenter(handle: QRadialGradientH; center: QPointFH); overload; cdecl; external QtIntf name 'QRadialGradient_setCenter';
procedure QRadialGradient_setCenter(handle: QRadialGradientH; x: Double; y: Double); overload; cdecl; external QtIntf name 'QRadialGradient_setCenter2';
procedure QRadialGradient_focalPoint(handle: QRadialGradientH; retval: QPointFH); cdecl; external QtIntf name 'QRadialGradient_focalPoint';
procedure QRadialGradient_setFocalPoint(handle: QRadialGradientH; focalPoint: QPointFH); overload; cdecl; external QtIntf name 'QRadialGradient_setFocalPoint';
procedure QRadialGradient_setFocalPoint(handle: QRadialGradientH; x: Double; y: Double); overload; cdecl; external QtIntf name 'QRadialGradient_setFocalPoint2';
function QRadialGradient_radius(handle: QRadialGradientH): Double; cdecl; external QtIntf name 'QRadialGradient_radius';
procedure QRadialGradient_setRadius(handle: QRadialGradientH; radius: Double); cdecl; external QtIntf name 'QRadialGradient_setRadius';

function QConicalGradient_create(): QConicalGradientH; overload; cdecl; external QtIntf name 'QConicalGradient_create';
procedure QConicalGradient_destroy(handle: QConicalGradientH); cdecl; external QtIntf name 'QConicalGradient_destroy'; 
function QConicalGradient_create(center: QPointFH; startAngle: Double): QConicalGradientH; overload; cdecl; external QtIntf name 'QConicalGradient_create2';
function QConicalGradient_create(cx: Double; cy: Double; startAngle: Double): QConicalGradientH; overload; cdecl; external QtIntf name 'QConicalGradient_create3';
procedure QConicalGradient_center(handle: QConicalGradientH; retval: QPointFH); cdecl; external QtIntf name 'QConicalGradient_center';
procedure QConicalGradient_setCenter(handle: QConicalGradientH; center: QPointFH); overload; cdecl; external QtIntf name 'QConicalGradient_setCenter';
procedure QConicalGradient_setCenter(handle: QConicalGradientH; x: Double; y: Double); overload; cdecl; external QtIntf name 'QConicalGradient_setCenter2';
function QConicalGradient_angle(handle: QConicalGradientH): Double; cdecl; external QtIntf name 'QConicalGradient_angle';
procedure QConicalGradient_setAngle(handle: QConicalGradientH; angle: Double); cdecl; external QtIntf name 'QConicalGradient_setAngle';

function QPen_create(): QPenH; overload; cdecl; external QtIntf name 'QPen_create';
procedure QPen_destroy(handle: QPenH); cdecl; external QtIntf name 'QPen_destroy'; 
function QPen_create(p1: QtPenStyle): QPenH; overload; cdecl; external QtIntf name 'QPen_create2';
function QPen_create(color: PQColor): QPenH; overload; cdecl; external QtIntf name 'QPen_create3';
function QPen_create(brush: QBrushH; width: Double; s: QtPenStyle = QtSolidLine; c: QtPenCapStyle = QtSquareCap; j: QtPenJoinStyle = QtBevelJoin): QPenH; overload; cdecl; external QtIntf name 'QPen_create4';
function QPen_create(pen: QPenH): QPenH; overload; cdecl; external QtIntf name 'QPen_create5';
function QPen_style(handle: QPenH): QtPenStyle; cdecl; external QtIntf name 'QPen_style';
procedure QPen_setStyle(handle: QPenH; p1: QtPenStyle); cdecl; external QtIntf name 'QPen_setStyle';
function QPen_miterLimit(handle: QPenH): Double; cdecl; external QtIntf name 'QPen_miterLimit';
procedure QPen_setMiterLimit(handle: QPenH; limit: Double); cdecl; external QtIntf name 'QPen_setMiterLimit';
function QPen_widthF(handle: QPenH): Double; cdecl; external QtIntf name 'QPen_widthF';
procedure QPen_setWidthF(handle: QPenH; width: Double); cdecl; external QtIntf name 'QPen_setWidthF';
function QPen_width(handle: QPenH): Integer; cdecl; external QtIntf name 'QPen_width';
procedure QPen_setWidth(handle: QPenH; width: Integer); cdecl; external QtIntf name 'QPen_setWidth';
procedure QPen_color(handle: QPenH; retval: PQColor); cdecl; external QtIntf name 'QPen_color';
procedure QPen_setColor(handle: QPenH; color: PQColor); cdecl; external QtIntf name 'QPen_setColor';
procedure QPen_brush(handle: QPenH; retval: QBrushH); cdecl; external QtIntf name 'QPen_brush';
procedure QPen_setBrush(handle: QPenH; brush: QBrushH); cdecl; external QtIntf name 'QPen_setBrush';
function QPen_isSolid(handle: QPenH): Boolean; cdecl; external QtIntf name 'QPen_isSolid';
function QPen_capStyle(handle: QPenH): QtPenCapStyle; cdecl; external QtIntf name 'QPen_capStyle';
procedure QPen_setCapStyle(handle: QPenH; pcs: QtPenCapStyle); cdecl; external QtIntf name 'QPen_setCapStyle';
function QPen_joinStyle(handle: QPenH): QtPenJoinStyle; cdecl; external QtIntf name 'QPen_joinStyle';
procedure QPen_setJoinStyle(handle: QPenH; pcs: QtPenJoinStyle); cdecl; external QtIntf name 'QPen_setJoinStyle';
function QPen_isDetached(handle: QPenH): Boolean; cdecl; external QtIntf name 'QPen_isDetached';

function QPolygon_create(): QPolygonH; overload; cdecl; external QtIntf name 'QPolygon_create';
procedure QPolygon_destroy(handle: QPolygonH); cdecl; external QtIntf name 'QPolygon_destroy'; 
function QPolygon_create(size: Integer): QPolygonH; overload; cdecl; external QtIntf name 'QPolygon_create2';
function QPolygon_create(a: QPolygonH): QPolygonH; overload; cdecl; external QtIntf name 'QPolygon_create3';
function QPolygon_create(r: PRect; closed: Boolean = False): QPolygonH; overload; cdecl; external QtIntf name 'QPolygon_create4';
function QPolygon_create(nPoints: Integer; points: PInteger): QPolygonH; overload; cdecl; external QtIntf name 'QPolygon_create5';
procedure QPolygon_translate(handle: QPolygonH; dx: Integer; dy: Integer); overload; cdecl; external QtIntf name 'QPolygon_translate';
procedure QPolygon_translate(handle: QPolygonH; offset: PQtPoint); overload; cdecl; external QtIntf name 'QPolygon_translate2';
procedure QPolygon_boundingRect(handle: QPolygonH; retval: PRect); cdecl; external QtIntf name 'QPolygon_boundingRect';
procedure QPolygon_point(handle: QPolygonH; i: Integer; x: PInteger; y: PInteger); overload; cdecl; external QtIntf name 'QPolygon_point';
procedure QPolygon_point(handle: QPolygonH; retval: PQtPoint; i: Integer); overload; cdecl; external QtIntf name 'QPolygon_point2';
procedure QPolygon_setPoint(handle: QPolygonH; index: Integer; x: Integer; y: Integer); overload; cdecl; external QtIntf name 'QPolygon_setPoint';
procedure QPolygon_setPoint(handle: QPolygonH; index: Integer; p: PQtPoint); overload; cdecl; external QtIntf name 'QPolygon_setPoint2';
procedure QPolygon_setPoints(handle: QPolygonH; nPoints: Integer; points: PInteger); overload; cdecl; external QtIntf name 'QPolygon_setPoints';
procedure QPolygon_putPoints(handle: QPolygonH; index: Integer; nPoints: Integer; points: PInteger); overload; cdecl; external QtIntf name 'QPolygon_putPoints';
procedure QPolygon_putPoints(handle: QPolygonH; index: Integer; nPoints: Integer; from: QPolygonH; fromIndex: Integer = 0); overload; cdecl; external QtIntf name 'QPolygon_putPoints3';

function QPolygonF_create(): QPolygonFH; overload; cdecl; external QtIntf name 'QPolygonF_create';
procedure QPolygonF_destroy(handle: QPolygonFH); cdecl; external QtIntf name 'QPolygonF_destroy'; 
function QPolygonF_create(size: Integer): QPolygonFH; overload; cdecl; external QtIntf name 'QPolygonF_create2';
function QPolygonF_create(a: QPolygonFH): QPolygonFH; overload; cdecl; external QtIntf name 'QPolygonF_create3';
function QPolygonF_create(r: QRectFH): QPolygonFH; overload; cdecl; external QtIntf name 'QPolygonF_create4';
function QPolygonF_create(a: QPolygonH): QPolygonFH; overload; cdecl; external QtIntf name 'QPolygonF_create5';
procedure QPolygonF_translate(handle: QPolygonFH; dx: Double; dy: Double); overload; cdecl; external QtIntf name 'QPolygonF_translate';
procedure QPolygonF_translate(handle: QPolygonFH; offset: QPointFH); overload; cdecl; external QtIntf name 'QPolygonF_translate2';
procedure QPolygonF_toPolygon(handle: QPolygonFH; retval: QPolygonH); cdecl; external QtIntf name 'QPolygonF_toPolygon';
function QPolygonF_isClosed(handle: QPolygonFH): Boolean; cdecl; external QtIntf name 'QPolygonF_isClosed';
procedure QPolygonF_boundingRect(handle: QPolygonFH; retval: QRectFH); cdecl; external QtIntf name 'QPolygonF_boundingRect';


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

function QPainter_create(): QPainterH; overload; cdecl; external QtIntf name 'QPainter_create';
procedure QPainter_destroy(handle: QPainterH); cdecl; external QtIntf name 'QPainter_destroy'; 
function QPainter_create(p1: QPaintDeviceH): QPainterH; overload; cdecl; external QtIntf name 'QPainter_create2';
function QPainter_device(handle: QPainterH): QPaintDeviceH; cdecl; external QtIntf name 'QPainter_device';
function QPainter_begin(handle: QPainterH; p1: QPaintDeviceH): Boolean; cdecl; external QtIntf name 'QPainter_begin';
function QPainter_end(handle: QPainterH): Boolean; cdecl; external QtIntf name 'QPainter_end';
function QPainter_isActive(handle: QPainterH): Boolean; cdecl; external QtIntf name 'QPainter_isActive';
procedure QPainter_initFrom(handle: QPainterH; widget: QWidgetH); cdecl; external QtIntf name 'QPainter_initFrom';
procedure QPainter_setCompositionMode(handle: QPainterH; mode: QPainterCompositionMode); cdecl; external QtIntf name 'QPainter_setCompositionMode';
function QPainter_compositionMode(handle: QPainterH): QPainterCompositionMode; cdecl; external QtIntf name 'QPainter_compositionMode';
function QPainter_font(handle: QPainterH): QFontH; cdecl; external QtIntf name 'QPainter_font';
procedure QPainter_setFont(handle: QPainterH; f: QFontH); cdecl; external QtIntf name 'QPainter_setFont';
procedure QPainter_fontMetrics(handle: QPainterH; retval: QFontMetricsH); cdecl; external QtIntf name 'QPainter_fontMetrics';
procedure QPainter_fontInfo(handle: QPainterH; retval: QFontInfoH); cdecl; external QtIntf name 'QPainter_fontInfo';
procedure QPainter_setPen(handle: QPainterH; color: PQColor); overload; cdecl; external QtIntf name 'QPainter_setPen';
procedure QPainter_setPen(handle: QPainterH; pen: QPenH); overload; cdecl; external QtIntf name 'QPainter_setPen2';
procedure QPainter_setPen(handle: QPainterH; style: QtPenStyle); overload; cdecl; external QtIntf name 'QPainter_setPen3';
function QPainter_pen(handle: QPainterH): QPenH; cdecl; external QtIntf name 'QPainter_pen';
procedure QPainter_setBrush(handle: QPainterH; brush: QBrushH); overload; cdecl; external QtIntf name 'QPainter_setBrush';
procedure QPainter_setBrush(handle: QPainterH; style: QtBrushStyle); overload; cdecl; external QtIntf name 'QPainter_setBrush2';
function QPainter_brush(handle: QPainterH): QBrushH; cdecl; external QtIntf name 'QPainter_brush';
procedure QPainter_setBackgroundMode(handle: QPainterH; mode: QtBGMode); cdecl; external QtIntf name 'QPainter_setBackgroundMode';
function QPainter_backgroundMode(handle: QPainterH): QtBGMode; cdecl; external QtIntf name 'QPainter_backgroundMode';
procedure QPainter_brushOrigin(handle: QPainterH; retval: PQtPoint); cdecl; external QtIntf name 'QPainter_brushOrigin';
procedure QPainter_setBrushOrigin(handle: QPainterH; x: Integer; y: Integer); overload; cdecl; external QtIntf name 'QPainter_setBrushOrigin';
procedure QPainter_setBrushOrigin(handle: QPainterH; p1: PQtPoint); overload; cdecl; external QtIntf name 'QPainter_setBrushOrigin2';
procedure QPainter_setBrushOrigin(handle: QPainterH; p1: QPointFH); overload; cdecl; external QtIntf name 'QPainter_setBrushOrigin3';
procedure QPainter_setBackground(handle: QPainterH; bg: QBrushH); cdecl; external QtIntf name 'QPainter_setBackground';
function QPainter_background(handle: QPainterH): QBrushH; cdecl; external QtIntf name 'QPainter_background';
function QPainter_opacity(handle: QPainterH): Double; cdecl; external QtIntf name 'QPainter_opacity';
procedure QPainter_setOpacity(handle: QPainterH; opacity: Double); cdecl; external QtIntf name 'QPainter_setOpacity';
procedure QPainter_clipRegion(handle: QPainterH; retval: QRegionH); cdecl; external QtIntf name 'QPainter_clipRegion';
procedure QPainter_clipPath(handle: QPainterH; retval: QPainterPathH); cdecl; external QtIntf name 'QPainter_clipPath';
procedure QPainter_setClipRect(handle: QPainterH; p1: QRectFH; op: QtClipOperation = QtReplaceClip); overload; cdecl; external QtIntf name 'QPainter_setClipRect';
procedure QPainter_setClipRect(handle: QPainterH; p1: PRect; op: QtClipOperation = QtReplaceClip); overload; cdecl; external QtIntf name 'QPainter_setClipRect2';
procedure QPainter_setClipRect(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; op: QtClipOperation = QtReplaceClip); overload; cdecl; external QtIntf name 'QPainter_setClipRect3';
procedure QPainter_setClipRegion(handle: QPainterH; p1: QRegionH; op: QtClipOperation = QtReplaceClip); cdecl; external QtIntf name 'QPainter_setClipRegion';
procedure QPainter_setClipPath(handle: QPainterH; path: QPainterPathH; op: QtClipOperation = QtReplaceClip); cdecl; external QtIntf name 'QPainter_setClipPath';
procedure QPainter_setClipping(handle: QPainterH; enable: Boolean); cdecl; external QtIntf name 'QPainter_setClipping';
function QPainter_hasClipping(handle: QPainterH): Boolean; cdecl; external QtIntf name 'QPainter_hasClipping';
procedure QPainter_save(handle: QPainterH); cdecl; external QtIntf name 'QPainter_save';
procedure QPainter_restore(handle: QPainterH); cdecl; external QtIntf name 'QPainter_restore';
procedure QPainter_setMatrix(handle: QPainterH; matrix: QMatrixH; combine: Boolean = False); cdecl; external QtIntf name 'QPainter_setMatrix';
function QPainter_matrix(handle: QPainterH): QMatrixH; cdecl; external QtIntf name 'QPainter_matrix';
function QPainter_deviceMatrix(handle: QPainterH): QMatrixH; cdecl; external QtIntf name 'QPainter_deviceMatrix';
procedure QPainter_resetMatrix(handle: QPainterH); cdecl; external QtIntf name 'QPainter_resetMatrix';
procedure QPainter_setWorldMatrix(handle: QPainterH; matrix: QMatrixH; combine: Boolean = False); cdecl; external QtIntf name 'QPainter_setWorldMatrix';
function QPainter_worldMatrix(handle: QPainterH): QMatrixH; cdecl; external QtIntf name 'QPainter_worldMatrix';
procedure QPainter_combinedMatrix(handle: QPainterH; retval: QMatrixH); cdecl; external QtIntf name 'QPainter_combinedMatrix';
procedure QPainter_setMatrixEnabled(handle: QPainterH; enabled: Boolean); cdecl; external QtIntf name 'QPainter_setMatrixEnabled';
function QPainter_matrixEnabled(handle: QPainterH): Boolean; cdecl; external QtIntf name 'QPainter_matrixEnabled';
procedure QPainter_setWorldMatrixEnabled(handle: QPainterH; enabled: Boolean); cdecl; external QtIntf name 'QPainter_setWorldMatrixEnabled';
function QPainter_worldMatrixEnabled(handle: QPainterH): Boolean; cdecl; external QtIntf name 'QPainter_worldMatrixEnabled';
procedure QPainter_scale(handle: QPainterH; sx: Double; sy: Double); cdecl; external QtIntf name 'QPainter_scale';
procedure QPainter_shear(handle: QPainterH; sh: Double; sv: Double); cdecl; external QtIntf name 'QPainter_shear';
procedure QPainter_rotate(handle: QPainterH; a: Double); cdecl; external QtIntf name 'QPainter_rotate';
procedure QPainter_translate(handle: QPainterH; offset: QPointFH); overload; cdecl; external QtIntf name 'QPainter_translate';
procedure QPainter_translate(handle: QPainterH; offset: PQtPoint); overload; cdecl; external QtIntf name 'QPainter_translate2';
procedure QPainter_translate(handle: QPainterH; dx: Double; dy: Double); overload; cdecl; external QtIntf name 'QPainter_translate3';
procedure QPainter_window(handle: QPainterH; retval: PRect); cdecl; external QtIntf name 'QPainter_window';
procedure QPainter_setWindow(handle: QPainterH; window: PRect); overload; cdecl; external QtIntf name 'QPainter_setWindow';
procedure QPainter_setWindow(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QPainter_setWindow2';
procedure QPainter_viewport(handle: QPainterH; retval: PRect); cdecl; external QtIntf name 'QPainter_viewport';
procedure QPainter_setViewport(handle: QPainterH; viewport: PRect); overload; cdecl; external QtIntf name 'QPainter_setViewport';
procedure QPainter_setViewport(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QPainter_setViewport2';
procedure QPainter_setViewTransformEnabled(handle: QPainterH; enable: Boolean); cdecl; external QtIntf name 'QPainter_setViewTransformEnabled';
function QPainter_viewTransformEnabled(handle: QPainterH): Boolean; cdecl; external QtIntf name 'QPainter_viewTransformEnabled';
procedure QPainter_strokePath(handle: QPainterH; path: QPainterPathH; pen: QPenH); cdecl; external QtIntf name 'QPainter_strokePath';
procedure QPainter_fillPath(handle: QPainterH; path: QPainterPathH; brush: QBrushH); cdecl; external QtIntf name 'QPainter_fillPath';
procedure QPainter_drawPath(handle: QPainterH; path: QPainterPathH); cdecl; external QtIntf name 'QPainter_drawPath';
procedure QPainter_drawPoint(handle: QPainterH; pt: QPointFH); overload; cdecl; external QtIntf name 'QPainter_drawPoint';
procedure QPainter_drawPoint(handle: QPainterH; p: PQtPoint); overload; cdecl; external QtIntf name 'QPainter_drawPoint2';
procedure QPainter_drawPoint(handle: QPainterH; x: Integer; y: Integer); overload; cdecl; external QtIntf name 'QPainter_drawPoint3';
procedure QPainter_drawPoints(handle: QPainterH; points: QPointFH; pointCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawPoints';
procedure QPainter_drawPoints(handle: QPainterH; points: QPolygonFH); overload; cdecl; external QtIntf name 'QPainter_drawPoints2';
procedure QPainter_drawPoints(handle: QPainterH; points: PQtPoint; pointCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawPoints3';
procedure QPainter_drawPoints(handle: QPainterH; points: QPolygonH); overload; cdecl; external QtIntf name 'QPainter_drawPoints4';
procedure QPainter_drawLine(handle: QPainterH; line: QLineFH); overload; cdecl; external QtIntf name 'QPainter_drawLine';
procedure QPainter_drawLine(handle: QPainterH; line: QLineH); overload; cdecl; external QtIntf name 'QPainter_drawLine2';
procedure QPainter_drawLine(handle: QPainterH; x1: Integer; y1: Integer; x2: Integer; y2: Integer); overload; cdecl; external QtIntf name 'QPainter_drawLine3';
procedure QPainter_drawLine(handle: QPainterH; p1: PQtPoint; p2: PQtPoint); overload; cdecl; external QtIntf name 'QPainter_drawLine4';
procedure QPainter_drawLine(handle: QPainterH; p1: QPointFH; p2: QPointFH); overload; cdecl; external QtIntf name 'QPainter_drawLine5';
procedure QPainter_drawLines(handle: QPainterH; lines: QLineFH; lineCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawLines';
procedure QPainter_drawLines(handle: QPainterH; pointPairs: QPointFH; lineCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawLines2';
procedure QPainter_drawLines(handle: QPainterH; lines: QLineH; lineCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawLines3';
procedure QPainter_drawLines(handle: QPainterH; pointPairs: PQtPoint; lineCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawLines4';
procedure QPainter_drawRect(handle: QPainterH; rect: QRectFH); overload; cdecl; external QtIntf name 'QPainter_drawRect';
procedure QPainter_drawRect(handle: QPainterH; x1: Integer; y1: Integer; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QPainter_drawRect2';
procedure QPainter_drawRect(handle: QPainterH; rect: PRect); overload; cdecl; external QtIntf name 'QPainter_drawRect3';
procedure QPainter_drawRects(handle: QPainterH; rects: QRectFH; rectCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawRects';
procedure QPainter_drawRects(handle: QPainterH; rects: PRect; rectCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawRects2';
procedure QPainter_drawEllipse(handle: QPainterH; r: QRectFH); overload; cdecl; external QtIntf name 'QPainter_drawEllipse';
procedure QPainter_drawEllipse(handle: QPainterH; r: PRect); overload; cdecl; external QtIntf name 'QPainter_drawEllipse2';
procedure QPainter_drawEllipse(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QPainter_drawEllipse3';
procedure QPainter_drawPolyline(handle: QPainterH; points: QPointFH; pointCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawPolyline';
procedure QPainter_drawPolyline(handle: QPainterH; polyline: QPolygonFH); overload; cdecl; external QtIntf name 'QPainter_drawPolyline2';
procedure QPainter_drawPolyline(handle: QPainterH; points: PQtPoint; pointCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawPolyline3';
procedure QPainter_drawPolyline(handle: QPainterH; polygon: QPolygonH); overload; cdecl; external QtIntf name 'QPainter_drawPolyline4';
procedure QPainter_drawPolygon(handle: QPainterH; points: QPointFH; pointCount: Integer; fillRule: QtFillRule = QtOddEvenFill); overload; cdecl; external QtIntf name 'QPainter_drawPolygon';
procedure QPainter_drawPolygon(handle: QPainterH; polygon: QPolygonFH; fillRule: QtFillRule = QtOddEvenFill); overload; cdecl; external QtIntf name 'QPainter_drawPolygon2';
procedure QPainter_drawPolygon(handle: QPainterH; points: PQtPoint; pointCount: Integer; fillRule: QtFillRule = QtOddEvenFill); overload; cdecl; external QtIntf name 'QPainter_drawPolygon3';
procedure QPainter_drawPolygon(handle: QPainterH; polygon: QPolygonH; fillRule: QtFillRule = QtOddEvenFill); overload; cdecl; external QtIntf name 'QPainter_drawPolygon4';
procedure QPainter_drawConvexPolygon(handle: QPainterH; points: QPointFH; pointCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawConvexPolygon';
procedure QPainter_drawConvexPolygon(handle: QPainterH; polygon: QPolygonFH); overload; cdecl; external QtIntf name 'QPainter_drawConvexPolygon2';
procedure QPainter_drawConvexPolygon(handle: QPainterH; points: PQtPoint; pointCount: Integer); overload; cdecl; external QtIntf name 'QPainter_drawConvexPolygon3';
procedure QPainter_drawConvexPolygon(handle: QPainterH; polygon: QPolygonH); overload; cdecl; external QtIntf name 'QPainter_drawConvexPolygon4';
procedure QPainter_drawArc(handle: QPainterH; rect: QRectFH; a: Integer; alen: Integer); overload; cdecl; external QtIntf name 'QPainter_drawArc';
procedure QPainter_drawArc(handle: QPainterH; p1: PRect; a: Integer; alen: Integer); overload; cdecl; external QtIntf name 'QPainter_drawArc2';
procedure QPainter_drawArc(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; a: Integer; alen: Integer); overload; cdecl; external QtIntf name 'QPainter_drawArc3';
procedure QPainter_drawPie(handle: QPainterH; rect: QRectFH; a: Integer; alen: Integer); overload; cdecl; external QtIntf name 'QPainter_drawPie';
procedure QPainter_drawPie(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; a: Integer; alen: Integer); overload; cdecl; external QtIntf name 'QPainter_drawPie2';
procedure QPainter_drawPie(handle: QPainterH; p1: PRect; a: Integer; alen: Integer); overload; cdecl; external QtIntf name 'QPainter_drawPie3';
procedure QPainter_drawChord(handle: QPainterH; rect: QRectFH; a: Integer; alen: Integer); overload; cdecl; external QtIntf name 'QPainter_drawChord';
procedure QPainter_drawChord(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; a: Integer; alen: Integer); overload; cdecl; external QtIntf name 'QPainter_drawChord2';
procedure QPainter_drawChord(handle: QPainterH; p1: PRect; a: Integer; alen: Integer); overload; cdecl; external QtIntf name 'QPainter_drawChord3';
procedure QPainter_drawRoundRect(handle: QPainterH; r: QRectFH; xround: Integer = 25; yround: Integer = 25); overload; cdecl; external QtIntf name 'QPainter_drawRoundRect';
procedure QPainter_drawRoundRect(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; p5: Integer = 25; p6: Integer = 25); overload; cdecl; external QtIntf name 'QPainter_drawRoundRect2';
procedure QPainter_drawRoundRect(handle: QPainterH; r: PRect; xround: Integer = 25; yround: Integer = 25); overload; cdecl; external QtIntf name 'QPainter_drawRoundRect3';
procedure QPainter_drawTiledPixmap(handle: QPainterH; rect: QRectFH; pm: QPixmapH; offset: QPointFH = nil); overload; cdecl; external QtIntf name 'QPainter_drawTiledPixmap';
procedure QPainter_drawTiledPixmap(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; p5: QPixmapH; sx: Integer = 0; sy: Integer = 0); overload; cdecl; external QtIntf name 'QPainter_drawTiledPixmap2';
procedure QPainter_drawTiledPixmap(handle: QPainterH; p1: PRect; p2: QPixmapH; p3: PQtPoint = nil); overload; cdecl; external QtIntf name 'QPainter_drawTiledPixmap3';
procedure QPainter_drawPicture(handle: QPainterH; p: QPointFH; picture: QPictureH); overload; cdecl; external QtIntf name 'QPainter_drawPicture';
procedure QPainter_drawPicture(handle: QPainterH; x: Integer; y: Integer; picture: QPictureH); overload; cdecl; external QtIntf name 'QPainter_drawPicture2';
procedure QPainter_drawPicture(handle: QPainterH; p: PQtPoint; picture: QPictureH); overload; cdecl; external QtIntf name 'QPainter_drawPicture3';
procedure QPainter_drawPixmap(handle: QPainterH; targetRect: QRectFH; pixmap: QPixmapH; sourceRect: QRectFH); overload; cdecl; external QtIntf name 'QPainter_drawPixmap';
procedure QPainter_drawPixmap(handle: QPainterH; targetRect: PRect; pixmap: QPixmapH; sourceRect: PRect); overload; cdecl; external QtIntf name 'QPainter_drawPixmap2';
procedure QPainter_drawPixmap(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; pm: QPixmapH; sx: Integer; sy: Integer; sw: Integer; sh: Integer); overload; cdecl; external QtIntf name 'QPainter_drawPixmap3';
procedure QPainter_drawPixmap(handle: QPainterH; x: Integer; y: Integer; pm: QPixmapH; sx: Integer; sy: Integer; sw: Integer; sh: Integer); overload; cdecl; external QtIntf name 'QPainter_drawPixmap4';
procedure QPainter_drawPixmap(handle: QPainterH; p: QPointFH; pm: QPixmapH; sr: QRectFH); overload; cdecl; external QtIntf name 'QPainter_drawPixmap5';
procedure QPainter_drawPixmap(handle: QPainterH; p: PQtPoint; pm: QPixmapH; sr: PRect); overload; cdecl; external QtIntf name 'QPainter_drawPixmap6';
procedure QPainter_drawPixmap(handle: QPainterH; p: QPointFH; pm: QPixmapH); overload; cdecl; external QtIntf name 'QPainter_drawPixmap7';
procedure QPainter_drawPixmap(handle: QPainterH; p: PQtPoint; pm: QPixmapH); overload; cdecl; external QtIntf name 'QPainter_drawPixmap8';
procedure QPainter_drawPixmap(handle: QPainterH; x: Integer; y: Integer; pm: QPixmapH); overload; cdecl; external QtIntf name 'QPainter_drawPixmap9';
procedure QPainter_drawPixmap(handle: QPainterH; r: PRect; pm: QPixmapH); overload; cdecl; external QtIntf name 'QPainter_drawPixmap10';
procedure QPainter_drawPixmap(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; pm: QPixmapH); overload; cdecl; external QtIntf name 'QPainter_drawPixmap11';
procedure QPainter_drawImage(handle: QPainterH; targetRect: QRectFH; image: QImageH; sourceRect: QRectFH; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtIntf name 'QPainter_drawImage';
procedure QPainter_drawImage(handle: QPainterH; targetRect: PRect; image: QImageH; sourceRect: PRect; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtIntf name 'QPainter_drawImage2';
procedure QPainter_drawImage(handle: QPainterH; p: QPointFH; image: QImageH; sr: QRectFH; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtIntf name 'QPainter_drawImage3';
procedure QPainter_drawImage(handle: QPainterH; p: PQtPoint; image: QImageH; sr: PRect; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtIntf name 'QPainter_drawImage4';
procedure QPainter_drawImage(handle: QPainterH; r: QRectFH; image: QImageH); overload; cdecl; external QtIntf name 'QPainter_drawImage5';
procedure QPainter_drawImage(handle: QPainterH; r: PRect; image: QImageH); overload; cdecl; external QtIntf name 'QPainter_drawImage6';
procedure QPainter_drawImage(handle: QPainterH; p: QPointFH; image: QImageH); overload; cdecl; external QtIntf name 'QPainter_drawImage7';
procedure QPainter_drawImage(handle: QPainterH; p: PQtPoint; image: QImageH); overload; cdecl; external QtIntf name 'QPainter_drawImage8';
procedure QPainter_drawImage(handle: QPainterH; x: Integer; y: Integer; image: QImageH; sx: Integer = 0; sy: Integer = 0; sw: Integer = -1; sh: Integer = -1; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtIntf name 'QPainter_drawImage9';
procedure QPainter_setLayoutDirection(handle: QPainterH; direction: QtLayoutDirection); cdecl; external QtIntf name 'QPainter_setLayoutDirection';
function QPainter_layoutDirection(handle: QPainterH): QtLayoutDirection; cdecl; external QtIntf name 'QPainter_layoutDirection';
procedure QPainter_drawText(handle: QPainterH; p: QPointFH; s: PWideString); overload; cdecl; external QtIntf name 'QPainter_drawText';
procedure QPainter_drawText(handle: QPainterH; p: PQtPoint; s: PWideString); overload; cdecl; external QtIntf name 'QPainter_drawText2';
procedure QPainter_drawText(handle: QPainterH; x: Integer; y: Integer; s: PWideString); overload; cdecl; external QtIntf name 'QPainter_drawText3';
procedure QPainter_drawText(handle: QPainterH; r: QRectFH; flags: Integer; text: PWideString; br: QRectFH = nil); overload; cdecl; external QtIntf name 'QPainter_drawText4';
procedure QPainter_drawText(handle: QPainterH; r: PRect; flags: Integer; text: PWideString; br: PRect = nil); overload; cdecl; external QtIntf name 'QPainter_drawText5';
procedure QPainter_drawText(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; flags: Integer; text: PWideString; br: PRect = nil); overload; cdecl; external QtIntf name 'QPainter_drawText6';
procedure QPainter_drawText(handle: QPainterH; r: QRectFH; text: PWideString; o: QTextOptionH = nil); overload; cdecl; external QtIntf name 'QPainter_drawText7';
procedure QPainter_boundingRect(handle: QPainterH; retval: QRectFH; rect: QRectFH; flags: Integer; text: PWideString); overload; cdecl; external QtIntf name 'QPainter_boundingRect';
procedure QPainter_boundingRect(handle: QPainterH; retval: PRect; rect: PRect; flags: Integer; text: PWideString); overload; cdecl; external QtIntf name 'QPainter_boundingRect2';
procedure QPainter_boundingRect(handle: QPainterH; retval: PRect; x: Integer; y: Integer; w: Integer; h: Integer; flags: Integer; text: PWideString); overload; cdecl; external QtIntf name 'QPainter_boundingRect3';
procedure QPainter_boundingRect(handle: QPainterH; retval: QRectFH; rect: QRectFH; text: PWideString; o: QTextOptionH = nil); overload; cdecl; external QtIntf name 'QPainter_boundingRect4';
procedure QPainter_fillRect(handle: QPainterH; p1: QRectFH; p2: QBrushH); overload; cdecl; external QtIntf name 'QPainter_fillRect';
procedure QPainter_fillRect(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; p5: QBrushH); overload; cdecl; external QtIntf name 'QPainter_fillRect2';
procedure QPainter_fillRect(handle: QPainterH; p1: PRect; p2: QBrushH); overload; cdecl; external QtIntf name 'QPainter_fillRect3';
procedure QPainter_eraseRect(handle: QPainterH; p1: QRectFH); overload; cdecl; external QtIntf name 'QPainter_eraseRect';
procedure QPainter_eraseRect(handle: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QPainter_eraseRect2';
procedure QPainter_eraseRect(handle: QPainterH; p1: PRect); overload; cdecl; external QtIntf name 'QPainter_eraseRect3';
procedure QPainter_setRenderHint(handle: QPainterH; hint: QPainterRenderHint; _on: Boolean = True); cdecl; external QtIntf name 'QPainter_setRenderHint';
procedure QPainter_setRenderHints(handle: QPainterH; hints: QPainterRenderHints; _on: Boolean = True); cdecl; external QtIntf name 'QPainter_setRenderHints';
function QPainter_renderHints(handle: QPainterH): QPainterRenderHints; cdecl; external QtIntf name 'QPainter_renderHints';
function QPainter_paintEngine(handle: QPainterH): QPaintEngineH; cdecl; external QtIntf name 'QPainter_paintEngine';
procedure QPainter_setRedirected(device: QPaintDeviceH; replacement: QPaintDeviceH; offset: PQtPoint = nil); cdecl; external QtIntf name 'QPainter_setRedirected';
function QPainter_redirected(device: QPaintDeviceH; offset: PQtPoint = nil): QPaintDeviceH; cdecl; external QtIntf name 'QPainter_redirected';
procedure QPainter_restoreRedirected(device: QPaintDeviceH); cdecl; external QtIntf name 'QPainter_restoreRedirected';


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
  QPaintEngineConstantOpacity =   $00001000;
  QPaintEngineMaskedBrush =   $00002000;
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
  QPaintEngineDirtyOpacity =   $1000;
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

function QPaintEngine_isActive(handle: QPaintEngineH): Boolean; cdecl; external QtIntf name 'QPaintEngine_isActive';
procedure QPaintEngine_setActive(handle: QPaintEngineH; newState: Boolean); cdecl; external QtIntf name 'QPaintEngine_setActive';
function QPaintEngine_begin(handle: QPaintEngineH; pdev: QPaintDeviceH): Boolean; cdecl; external QtIntf name 'QPaintEngine_begin';
function QPaintEngine_end(handle: QPaintEngineH): Boolean; cdecl; external QtIntf name 'QPaintEngine_end';
procedure QPaintEngine_drawRects(handle: QPaintEngineH; rects: PRect; rectCount: Integer); overload; cdecl; external QtIntf name 'QPaintEngine_drawRects';
procedure QPaintEngine_drawRects(handle: QPaintEngineH; rects: QRectFH; rectCount: Integer); overload; cdecl; external QtIntf name 'QPaintEngine_drawRects2';
procedure QPaintEngine_drawLines(handle: QPaintEngineH; lines: QLineH; lineCount: Integer); overload; cdecl; external QtIntf name 'QPaintEngine_drawLines';
procedure QPaintEngine_drawLines(handle: QPaintEngineH; lines: QLineFH; lineCount: Integer); overload; cdecl; external QtIntf name 'QPaintEngine_drawLines2';
procedure QPaintEngine_drawEllipse(handle: QPaintEngineH; r: QRectFH); overload; cdecl; external QtIntf name 'QPaintEngine_drawEllipse';
procedure QPaintEngine_drawEllipse(handle: QPaintEngineH; r: PRect); overload; cdecl; external QtIntf name 'QPaintEngine_drawEllipse2';
procedure QPaintEngine_drawPath(handle: QPaintEngineH; path: QPainterPathH); cdecl; external QtIntf name 'QPaintEngine_drawPath';
procedure QPaintEngine_drawPoints(handle: QPaintEngineH; points: QPointFH; pointCount: Integer); overload; cdecl; external QtIntf name 'QPaintEngine_drawPoints';
procedure QPaintEngine_drawPoints(handle: QPaintEngineH; points: PQtPoint; pointCount: Integer); overload; cdecl; external QtIntf name 'QPaintEngine_drawPoints2';
procedure QPaintEngine_drawPolygon(handle: QPaintEngineH; points: QPointFH; pointCount: Integer; mode: QPaintEnginePolygonDrawMode); overload; cdecl; external QtIntf name 'QPaintEngine_drawPolygon';
procedure QPaintEngine_drawPolygon(handle: QPaintEngineH; points: PQtPoint; pointCount: Integer; mode: QPaintEnginePolygonDrawMode); overload; cdecl; external QtIntf name 'QPaintEngine_drawPolygon2';
procedure QPaintEngine_drawPixmap(handle: QPaintEngineH; r: QRectFH; pm: QPixmapH; sr: QRectFH); cdecl; external QtIntf name 'QPaintEngine_drawPixmap';
procedure QPaintEngine_drawTiledPixmap(handle: QPaintEngineH; r: QRectFH; pixmap: QPixmapH; s: QPointFH); cdecl; external QtIntf name 'QPaintEngine_drawTiledPixmap';
procedure QPaintEngine_drawImage(handle: QPaintEngineH; r: QRectFH; pm: QImageH; sr: QRectFH; flags: QtImageConversionFlags = QtAutoColor); cdecl; external QtIntf name 'QPaintEngine_drawImage';
procedure QPaintEngine_setPaintDevice(handle: QPaintEngineH; device: QPaintDeviceH); cdecl; external QtIntf name 'QPaintEngine_setPaintDevice';
function QPaintEngine_paintDevice(handle: QPaintEngineH): QPaintDeviceH; cdecl; external QtIntf name 'QPaintEngine_paintDevice';
procedure QPaintEngine_setSystemClip(handle: QPaintEngineH; baseClip: QRegionH); cdecl; external QtIntf name 'QPaintEngine_setSystemClip';
procedure QPaintEngine_systemClip(handle: QPaintEngineH; retval: QRegionH); cdecl; external QtIntf name 'QPaintEngine_systemClip';
procedure QPaintEngine_setSystemRect(handle: QPaintEngineH; rect: PRect); cdecl; external QtIntf name 'QPaintEngine_setSystemRect';
procedure QPaintEngine_systemRect(handle: QPaintEngineH; retval: PRect); cdecl; external QtIntf name 'QPaintEngine_systemRect';
procedure QPaintEngine_coordinateOffset(handle: QPaintEngineH; retval: PQtPoint); cdecl; external QtIntf name 'QPaintEngine_coordinateOffset';
function QPaintEngine_type(handle: QPaintEngineH): QPaintEngineType; cdecl; external QtIntf name 'QPaintEngine_type';
procedure QPaintEngine_fix_neg_rect(handle: QPaintEngineH; x: PInteger; y: PInteger; w: PInteger; h: PInteger); cdecl; external QtIntf name 'QPaintEngine_fix_neg_rect';
function QPaintEngine_testDirty(handle: QPaintEngineH; df: QPaintEngineDirtyFlags): Boolean; cdecl; external QtIntf name 'QPaintEngine_testDirty';
procedure QPaintEngine_setDirty(handle: QPaintEngineH; df: QPaintEngineDirtyFlags); cdecl; external QtIntf name 'QPaintEngine_setDirty';
procedure QPaintEngine_clearDirty(handle: QPaintEngineH; df: QPaintEngineDirtyFlags); cdecl; external QtIntf name 'QPaintEngine_clearDirty';
function QPaintEngine_hasFeature(handle: QPaintEngineH; feature: QPaintEnginePaintEngineFeatures): Boolean; cdecl; external QtIntf name 'QPaintEngine_hasFeature';
function QPaintEngine_painter(handle: QPaintEngineH): QPainterH; cdecl; external QtIntf name 'QPaintEngine_painter';
procedure QPaintEngine_syncState(handle: QPaintEngineH); cdecl; external QtIntf name 'QPaintEngine_syncState';
{$ifdef MSWINDOWS }
function QPaintEngine_getDC(handle: QPaintEngineH): HDC; cdecl; external QtIntf name 'QPaintEngine_getDC';
procedure QPaintEngine_releaseDC(handle: QPaintEngineH; hdc: HDC); cdecl; external QtIntf name 'QPaintEngine_releaseDC';
{$endif}


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

function QPaintDevice_devType(handle: QPaintDeviceH): Integer; cdecl; external QtIntf name 'QPaintDevice_devType';
function QPaintDevice_paintingActive(handle: QPaintDeviceH): Boolean; cdecl; external QtIntf name 'QPaintDevice_paintingActive';
function QPaintDevice_paintEngine(handle: QPaintDeviceH): QPaintEngineH; cdecl; external QtIntf name 'QPaintDevice_paintEngine';
function QPaintDevice_width(handle: QPaintDeviceH): Integer; cdecl; external QtIntf name 'QPaintDevice_width';
function QPaintDevice_height(handle: QPaintDeviceH): Integer; cdecl; external QtIntf name 'QPaintDevice_height';
function QPaintDevice_widthMM(handle: QPaintDeviceH): Integer; cdecl; external QtIntf name 'QPaintDevice_widthMM';
function QPaintDevice_heightMM(handle: QPaintDeviceH): Integer; cdecl; external QtIntf name 'QPaintDevice_heightMM';
function QPaintDevice_logicalDpiX(handle: QPaintDeviceH): Integer; cdecl; external QtIntf name 'QPaintDevice_logicalDpiX';
function QPaintDevice_logicalDpiY(handle: QPaintDeviceH): Integer; cdecl; external QtIntf name 'QPaintDevice_logicalDpiY';
function QPaintDevice_physicalDpiX(handle: QPaintDeviceH): Integer; cdecl; external QtIntf name 'QPaintDevice_physicalDpiX';
function QPaintDevice_physicalDpiY(handle: QPaintDeviceH): Integer; cdecl; external QtIntf name 'QPaintDevice_physicalDpiY';
function QPaintDevice_numColors(handle: QPaintDeviceH): Integer; cdecl; external QtIntf name 'QPaintDevice_numColors';
function QPaintDevice_depth(handle: QPaintDeviceH): Integer; cdecl; external QtIntf name 'QPaintDevice_depth';
{$ifdef MSWINDOWS }
function QPaintDevice_getDC(handle: QPaintDeviceH): HDC; cdecl; external QtIntf name 'QPaintDevice_getDC';
procedure QPaintDevice_releaseDC(handle: QPaintDeviceH; hdc: HDC); cdecl; external QtIntf name 'QPaintDevice_releaseDC';
{$endif}


type
  QRegionRegionType = ( // QRegion::RegionType (1)
    QRegionRectangle, QRegionEllipse );

function QRegion_create(): QRegionH; overload; cdecl; external QtIntf name 'QRegion_create';
procedure QRegion_destroy(handle: QRegionH); cdecl; external QtIntf name 'QRegion_destroy'; 
function QRegion_create(x: Integer; y: Integer; w: Integer; h: Integer; t: QRegionRegionType = QRegionRectangle): QRegionH; overload; cdecl; external QtIntf name 'QRegion_create2';
function QRegion_create(r: PRect; t: QRegionRegionType = QRegionRectangle): QRegionH; overload; cdecl; external QtIntf name 'QRegion_create3';
function QRegion_create(pa: QPolygonH; fillRule: QtFillRule = QtOddEvenFill): QRegionH; overload; cdecl; external QtIntf name 'QRegion_create4';
function QRegion_create(region: QRegionH): QRegionH; overload; cdecl; external QtIntf name 'QRegion_create5';
function QRegion_create(bitmap: QBitmapH): QRegionH; overload; cdecl; external QtIntf name 'QRegion_create6';
function QRegion_isEmpty(handle: QRegionH): Boolean; cdecl; external QtIntf name 'QRegion_isEmpty';
function QRegion_contains(handle: QRegionH; p: PQtPoint): Boolean; overload; cdecl; external QtIntf name 'QRegion_contains';
function QRegion_contains(handle: QRegionH; r: PRect): Boolean; overload; cdecl; external QtIntf name 'QRegion_contains2';
procedure QRegion_translate(handle: QRegionH; dx: Integer; dy: Integer); overload; cdecl; external QtIntf name 'QRegion_translate';
procedure QRegion_translate(handle: QRegionH; p: PQtPoint); overload; cdecl; external QtIntf name 'QRegion_translate2';
procedure QRegion_translated(handle: QRegionH; retval: QRegionH; dx: Integer; dy: Integer); overload; cdecl; external QtIntf name 'QRegion_translated';
procedure QRegion_translated(handle: QRegionH; retval: QRegionH; p: PQtPoint); overload; cdecl; external QtIntf name 'QRegion_translated2';
procedure QRegion_unite(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtIntf name 'QRegion_unite';
procedure QRegion_intersect(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtIntf name 'QRegion_intersect';
procedure QRegion_subtract(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtIntf name 'QRegion_subtract';
procedure QRegion_eor(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtIntf name 'QRegion_eor';
procedure QRegion_united(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtIntf name 'QRegion_united';
procedure QRegion_intersected(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtIntf name 'QRegion_intersected';
procedure QRegion_subtracted(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtIntf name 'QRegion_subtracted';
procedure QRegion_xored(handle: QRegionH; retval: QRegionH; r: QRegionH); cdecl; external QtIntf name 'QRegion_xored';
function QRegion_intersects(handle: QRegionH; r: QRegionH): Boolean; overload; cdecl; external QtIntf name 'QRegion_intersects';
function QRegion_intersects(handle: QRegionH; r: PRect): Boolean; overload; cdecl; external QtIntf name 'QRegion_intersects2';
procedure QRegion_boundingRect(handle: QRegionH; retval: PRect); cdecl; external QtIntf name 'QRegion_boundingRect';
procedure QRegion_setRects(handle: QRegionH; rect: PRect; num: Integer); cdecl; external QtIntf name 'QRegion_setRects';
{$ifdef UNIX }
function QRegion_handle(handle: QRegionH): Region; overload; cdecl; external QtIntf name 'QRegion_handle';
{$endif}
{$ifdef MSWINDOWS }
function QRegion_handle(handle: QRegionH): HRGN; overload; cdecl; external QtIntf name 'QRegion_handle2';
{$endif}
{$ifdef DARWIN }
function QRegion_handle(handle: QRegionH): RgnHandle; overload; cdecl; external QtIntf name 'QRegion_handle3';
function QRegion_handle(handle: QRegionH; require_rgn: Boolean): RgnHandle; overload; cdecl; external QtIntf name 'QRegion_handle4';
{$endif}


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
    QPrinterNativeFormat, QPrinterPdfFormat, QPrinterPostScriptFormat );

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


function QPrinter_create(mode: QPrinterPrinterMode = QPrinterScreenResolution): QPrinterH; cdecl; external QtIntf name 'QPrinter_create';
procedure QPrinter_destroy(handle: QPrinterH); cdecl; external QtIntf name 'QPrinter_destroy'; 
function QPrinter_devType(handle: QPrinterH): Integer; cdecl; external QtIntf name 'QPrinter_devType';
procedure QPrinter_setOutputFormat(handle: QPrinterH; format: QPrinterOutputFormat); cdecl; external QtIntf name 'QPrinter_setOutputFormat';
function QPrinter_outputFormat(handle: QPrinterH): QPrinterOutputFormat; cdecl; external QtIntf name 'QPrinter_outputFormat';
procedure QPrinter_setPrinterName(handle: QPrinterH; p1: PWideString); cdecl; external QtIntf name 'QPrinter_setPrinterName';
procedure QPrinter_printerName(handle: QPrinterH; retval: PWideString); cdecl; external QtIntf name 'QPrinter_printerName';
procedure QPrinter_setOutputFileName(handle: QPrinterH; p1: PWideString); cdecl; external QtIntf name 'QPrinter_setOutputFileName';
procedure QPrinter_outputFileName(handle: QPrinterH; retval: PWideString); cdecl; external QtIntf name 'QPrinter_outputFileName';
procedure QPrinter_setPrintProgram(handle: QPrinterH; p1: PWideString); cdecl; external QtIntf name 'QPrinter_setPrintProgram';
procedure QPrinter_printProgram(handle: QPrinterH; retval: PWideString); cdecl; external QtIntf name 'QPrinter_printProgram';
procedure QPrinter_setDocName(handle: QPrinterH; p1: PWideString); cdecl; external QtIntf name 'QPrinter_setDocName';
procedure QPrinter_docName(handle: QPrinterH; retval: PWideString); cdecl; external QtIntf name 'QPrinter_docName';
procedure QPrinter_setCreator(handle: QPrinterH; p1: PWideString); cdecl; external QtIntf name 'QPrinter_setCreator';
procedure QPrinter_creator(handle: QPrinterH; retval: PWideString); cdecl; external QtIntf name 'QPrinter_creator';
procedure QPrinter_setOrientation(handle: QPrinterH; p1: QPrinterOrientation); cdecl; external QtIntf name 'QPrinter_setOrientation';
function QPrinter_orientation(handle: QPrinterH): QPrinterOrientation; cdecl; external QtIntf name 'QPrinter_orientation';
procedure QPrinter_setPageSize(handle: QPrinterH; p1: QPrinterPageSize); cdecl; external QtIntf name 'QPrinter_setPageSize';
function QPrinter_pageSize(handle: QPrinterH): QPrinterPageSize; cdecl; external QtIntf name 'QPrinter_pageSize';
procedure QPrinter_setPageOrder(handle: QPrinterH; p1: QPrinterPageOrder); cdecl; external QtIntf name 'QPrinter_setPageOrder';
function QPrinter_pageOrder(handle: QPrinterH): QPrinterPageOrder; cdecl; external QtIntf name 'QPrinter_pageOrder';
procedure QPrinter_setResolution(handle: QPrinterH; p1: Integer); cdecl; external QtIntf name 'QPrinter_setResolution';
function QPrinter_resolution(handle: QPrinterH): Integer; cdecl; external QtIntf name 'QPrinter_resolution';
procedure QPrinter_setColorMode(handle: QPrinterH; p1: QPrinterColorMode); cdecl; external QtIntf name 'QPrinter_setColorMode';
function QPrinter_colorMode(handle: QPrinterH): QPrinterColorMode; cdecl; external QtIntf name 'QPrinter_colorMode';
procedure QPrinter_setCollateCopies(handle: QPrinterH; collate: Boolean); cdecl; external QtIntf name 'QPrinter_setCollateCopies';
function QPrinter_collateCopies(handle: QPrinterH): Boolean; cdecl; external QtIntf name 'QPrinter_collateCopies';
procedure QPrinter_setFullPage(handle: QPrinterH; p1: Boolean); cdecl; external QtIntf name 'QPrinter_setFullPage';
function QPrinter_fullPage(handle: QPrinterH): Boolean; cdecl; external QtIntf name 'QPrinter_fullPage';
procedure QPrinter_setNumCopies(handle: QPrinterH; p1: Integer); cdecl; external QtIntf name 'QPrinter_setNumCopies';
function QPrinter_numCopies(handle: QPrinterH): Integer; cdecl; external QtIntf name 'QPrinter_numCopies';
procedure QPrinter_setPaperSource(handle: QPrinterH; p1: QPrinterPaperSource); cdecl; external QtIntf name 'QPrinter_setPaperSource';
function QPrinter_paperSource(handle: QPrinterH): QPrinterPaperSource; cdecl; external QtIntf name 'QPrinter_paperSource';
procedure QPrinter_supportedResolutions(handle: QPrinterH; retval: PIntArray); cdecl; external QtIntf name 'QPrinter_supportedResolutions';
procedure QPrinter_setFontEmbeddingEnabled(handle: QPrinterH; enable: Boolean); cdecl; external QtIntf name 'QPrinter_setFontEmbeddingEnabled';
function QPrinter_fontEmbeddingEnabled(handle: QPrinterH): Boolean; cdecl; external QtIntf name 'QPrinter_fontEmbeddingEnabled';
procedure QPrinter_setDoubleSidedPrinting(handle: QPrinterH; enable: Boolean); cdecl; external QtIntf name 'QPrinter_setDoubleSidedPrinting';
function QPrinter_doubleSidedPrinting(handle: QPrinterH): Boolean; cdecl; external QtIntf name 'QPrinter_doubleSidedPrinting';
procedure QPrinter_paperRect(handle: QPrinterH; retval: PRect); cdecl; external QtIntf name 'QPrinter_paperRect';
procedure QPrinter_pageRect(handle: QPrinterH; retval: PRect); cdecl; external QtIntf name 'QPrinter_pageRect';
{$ifdef UNIX or DARWIN }
procedure QPrinter_printerSelectionOption(handle: QPrinterH; retval: PWideString); cdecl; external QtIntf name 'QPrinter_printerSelectionOption';
procedure QPrinter_setPrinterSelectionOption(handle: QPrinterH; p1: PWideString); cdecl; external QtIntf name 'QPrinter_setPrinterSelectionOption';
{$endif}
function QPrinter_newPage(handle: QPrinterH): Boolean; cdecl; external QtIntf name 'QPrinter_newPage';
function QPrinter_abort(handle: QPrinterH): Boolean; cdecl; external QtIntf name 'QPrinter_abort';
function QPrinter_printerState(handle: QPrinterH): QPrinterPrinterState; cdecl; external QtIntf name 'QPrinter_printerState';
function QPrinter_paintEngine(handle: QPrinterH): QPaintEngineH; cdecl; external QtIntf name 'QPrinter_paintEngine';
function QPrinter_printEngine(handle: QPrinterH): QPrintEngineH; cdecl; external QtIntf name 'QPrinter_printEngine';
procedure QPrinter_setFromTo(handle: QPrinterH; fromPage: Integer; toPage: Integer); cdecl; external QtIntf name 'QPrinter_setFromTo';
function QPrinter_fromPage(handle: QPrinterH): Integer; cdecl; external QtIntf name 'QPrinter_fromPage';
function QPrinter_toPage(handle: QPrinterH): Integer; cdecl; external QtIntf name 'QPrinter_toPage';
procedure QPrinter_setPrintRange(handle: QPrinterH; range: QPrinterPrintRange); cdecl; external QtIntf name 'QPrinter_setPrintRange';
function QPrinter_printRange(handle: QPrinterH): QPrinterPrintRange; cdecl; external QtIntf name 'QPrinter_printRange';
{$ifdef MSWINDOWS }
procedure QPrinter_setWinPageSize(handle: QPrinterH; winPageSize: Integer); cdecl; external QtIntf name 'QPrinter_setWinPageSize';
function QPrinter_winPageSize(handle: QPrinterH): Integer; cdecl; external QtIntf name 'QPrinter_winPageSize';
function QPrinter_getDC(handle: QPrinterH): HDC; cdecl; external QtIntf name 'QPrinter_getDC';
procedure QPrinter_releaseDC(handle: QPrinterH; hdc: HDC); cdecl; external QtIntf name 'QPrinter_releaseDC';
{$endif}


type
  QPainterPathElementType = ( // QPainterPath::ElementType (1)
    QPainterPathMoveToElement, QPainterPathLineToElement, QPainterPathCurveToElement, QPainterPathCurveToDataElement );

function QPainterPath_create(): QPainterPathH; overload; cdecl; external QtIntf name 'QPainterPath_create';
procedure QPainterPath_destroy(handle: QPainterPathH); cdecl; external QtIntf name 'QPainterPath_destroy'; 
function QPainterPath_create(startPoint: QPointFH): QPainterPathH; overload; cdecl; external QtIntf name 'QPainterPath_create2';
function QPainterPath_create(other: QPainterPathH): QPainterPathH; overload; cdecl; external QtIntf name 'QPainterPath_create3';
procedure QPainterPath_closeSubpath(handle: QPainterPathH); cdecl; external QtIntf name 'QPainterPath_closeSubpath';
procedure QPainterPath_moveTo(handle: QPainterPathH; p: QPointFH); overload; cdecl; external QtIntf name 'QPainterPath_moveTo';
procedure QPainterPath_moveTo(handle: QPainterPathH; x: Double; y: Double); overload; cdecl; external QtIntf name 'QPainterPath_moveTo2';
procedure QPainterPath_lineTo(handle: QPainterPathH; p: QPointFH); overload; cdecl; external QtIntf name 'QPainterPath_lineTo';
procedure QPainterPath_lineTo(handle: QPainterPathH; x: Double; y: Double); overload; cdecl; external QtIntf name 'QPainterPath_lineTo2';
procedure QPainterPath_arcMoveTo(handle: QPainterPathH; rect: QRectFH; angle: Double); overload; cdecl; external QtIntf name 'QPainterPath_arcMoveTo';
procedure QPainterPath_arcMoveTo(handle: QPainterPathH; x: Double; y: Double; w: Double; h: Double; angle: Double); overload; cdecl; external QtIntf name 'QPainterPath_arcMoveTo2';
procedure QPainterPath_arcTo(handle: QPainterPathH; rect: QRectFH; startAngle: Double; arcLength: Double); overload; cdecl; external QtIntf name 'QPainterPath_arcTo';
procedure QPainterPath_arcTo(handle: QPainterPathH; x: Double; y: Double; w: Double; h: Double; startAngle: Double; arcLength: Double); overload; cdecl; external QtIntf name 'QPainterPath_arcTo2';
procedure QPainterPath_cubicTo(handle: QPainterPathH; ctrlPt1: QPointFH; ctrlPt2: QPointFH; endPt: QPointFH); overload; cdecl; external QtIntf name 'QPainterPath_cubicTo';
procedure QPainterPath_cubicTo(handle: QPainterPathH; ctrlPt1x: Double; ctrlPt1y: Double; ctrlPt2x: Double; ctrlPt2y: Double; endPtx: Double; endPty: Double); overload; cdecl; external QtIntf name 'QPainterPath_cubicTo2';
procedure QPainterPath_quadTo(handle: QPainterPathH; ctrlPt: QPointFH; endPt: QPointFH); overload; cdecl; external QtIntf name 'QPainterPath_quadTo';
procedure QPainterPath_quadTo(handle: QPainterPathH; ctrlPtx: Double; ctrlPty: Double; endPtx: Double; endPty: Double); overload; cdecl; external QtIntf name 'QPainterPath_quadTo2';
procedure QPainterPath_currentPosition(handle: QPainterPathH; retval: QPointFH); cdecl; external QtIntf name 'QPainterPath_currentPosition';
procedure QPainterPath_addRect(handle: QPainterPathH; rect: QRectFH); overload; cdecl; external QtIntf name 'QPainterPath_addRect';
procedure QPainterPath_addRect(handle: QPainterPathH; x: Double; y: Double; w: Double; h: Double); overload; cdecl; external QtIntf name 'QPainterPath_addRect2';
procedure QPainterPath_addEllipse(handle: QPainterPathH; rect: QRectFH); overload; cdecl; external QtIntf name 'QPainterPath_addEllipse';
procedure QPainterPath_addEllipse(handle: QPainterPathH; x: Double; y: Double; w: Double; h: Double); overload; cdecl; external QtIntf name 'QPainterPath_addEllipse2';
procedure QPainterPath_addPolygon(handle: QPainterPathH; polygon: QPolygonFH); cdecl; external QtIntf name 'QPainterPath_addPolygon';
procedure QPainterPath_addText(handle: QPainterPathH; point: QPointFH; f: QFontH; text: PWideString); overload; cdecl; external QtIntf name 'QPainterPath_addText';
procedure QPainterPath_addText(handle: QPainterPathH; x: Double; y: Double; f: QFontH; text: PWideString); overload; cdecl; external QtIntf name 'QPainterPath_addText2';
procedure QPainterPath_addPath(handle: QPainterPathH; path: QPainterPathH); cdecl; external QtIntf name 'QPainterPath_addPath';
procedure QPainterPath_addRegion(handle: QPainterPathH; region: QRegionH); cdecl; external QtIntf name 'QPainterPath_addRegion';
procedure QPainterPath_connectPath(handle: QPainterPathH; path: QPainterPathH); cdecl; external QtIntf name 'QPainterPath_connectPath';
function QPainterPath_contains(handle: QPainterPathH; pt: QPointFH): Boolean; overload; cdecl; external QtIntf name 'QPainterPath_contains';
function QPainterPath_contains(handle: QPainterPathH; rect: QRectFH): Boolean; overload; cdecl; external QtIntf name 'QPainterPath_contains2';
function QPainterPath_intersects(handle: QPainterPathH; rect: QRectFH): Boolean; cdecl; external QtIntf name 'QPainterPath_intersects';
procedure QPainterPath_boundingRect(handle: QPainterPathH; retval: QRectFH); cdecl; external QtIntf name 'QPainterPath_boundingRect';
procedure QPainterPath_controlPointRect(handle: QPainterPathH; retval: QRectFH); cdecl; external QtIntf name 'QPainterPath_controlPointRect';
function QPainterPath_fillRule(handle: QPainterPathH): QtFillRule; cdecl; external QtIntf name 'QPainterPath_fillRule';
procedure QPainterPath_setFillRule(handle: QPainterPathH; fillRule: QtFillRule); cdecl; external QtIntf name 'QPainterPath_setFillRule';
function QPainterPath_isEmpty(handle: QPainterPathH): Boolean; cdecl; external QtIntf name 'QPainterPath_isEmpty';
procedure QPainterPath_toReversed(handle: QPainterPathH; retval: QPainterPathH); cdecl; external QtIntf name 'QPainterPath_toReversed';
procedure QPainterPath_toFillPolygon(handle: QPainterPathH; retval: QPolygonFH; matrix: QMatrixH = nil); cdecl; external QtIntf name 'QPainterPath_toFillPolygon';
function QPainterPath_elementCount(handle: QPainterPathH): Integer; cdecl; external QtIntf name 'QPainterPath_elementCount';
function QPainterPath_elementAt(handle: QPainterPathH; i: Integer): QPainterPathElementH; cdecl; external QtIntf name 'QPainterPath_elementAt';
procedure QPainterPath_setElementPositionAt(handle: QPainterPathH; i: Integer; x: Double; y: Double); cdecl; external QtIntf name 'QPainterPath_setElementPositionAt';


function QPainterPathStroker_create(): QPainterPathStrokerH; cdecl; external QtIntf name 'QPainterPathStroker_create';
procedure QPainterPathStroker_destroy(handle: QPainterPathStrokerH); cdecl; external QtIntf name 'QPainterPathStroker_destroy'; 
procedure QPainterPathStroker_setWidth(handle: QPainterPathStrokerH; width: Double); cdecl; external QtIntf name 'QPainterPathStroker_setWidth';
function QPainterPathStroker_width(handle: QPainterPathStrokerH): Double; cdecl; external QtIntf name 'QPainterPathStroker_width';
procedure QPainterPathStroker_setCapStyle(handle: QPainterPathStrokerH; style: QtPenCapStyle); cdecl; external QtIntf name 'QPainterPathStroker_setCapStyle';
function QPainterPathStroker_capStyle(handle: QPainterPathStrokerH): QtPenCapStyle; cdecl; external QtIntf name 'QPainterPathStroker_capStyle';
procedure QPainterPathStroker_setJoinStyle(handle: QPainterPathStrokerH; style: QtPenJoinStyle); cdecl; external QtIntf name 'QPainterPathStroker_setJoinStyle';
function QPainterPathStroker_joinStyle(handle: QPainterPathStrokerH): QtPenJoinStyle; cdecl; external QtIntf name 'QPainterPathStroker_joinStyle';
procedure QPainterPathStroker_setMiterLimit(handle: QPainterPathStrokerH; length: Double); cdecl; external QtIntf name 'QPainterPathStroker_setMiterLimit';
function QPainterPathStroker_miterLimit(handle: QPainterPathStrokerH): Double; cdecl; external QtIntf name 'QPainterPathStroker_miterLimit';
procedure QPainterPathStroker_setCurveThreshold(handle: QPainterPathStrokerH; threshold: Double); cdecl; external QtIntf name 'QPainterPathStroker_setCurveThreshold';
function QPainterPathStroker_curveThreshold(handle: QPainterPathStrokerH): Double; cdecl; external QtIntf name 'QPainterPathStroker_curveThreshold';
procedure QPainterPathStroker_setDashPattern(handle: QPainterPathStrokerH; p1: QtPenStyle); cdecl; external QtIntf name 'QPainterPathStroker_setDashPattern';
procedure QPainterPathStroker_createStroke(handle: QPainterPathStrokerH; retval: QPainterPathH; path: QPainterPathH); cdecl; external QtIntf name 'QPainterPathStroker_createStroke';


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
    QFontOpenGLCompatible = $0200,
    QFontNoFontMerging = $8000 );

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


function QFont_create(): QFontH; overload; cdecl; external QtIntf name 'QFont_create';
procedure QFont_destroy(handle: QFontH); cdecl; external QtIntf name 'QFont_destroy'; 
function QFont_create(family: PWideString; pointSize: Integer = -1; weight: Integer = -1; italic: Boolean = False): QFontH; overload; cdecl; external QtIntf name 'QFont_create2';
function QFont_create(p1: QFontH; pd: QPaintDeviceH): QFontH; overload; cdecl; external QtIntf name 'QFont_create3';
function QFont_create(p1: QFontH): QFontH; overload; cdecl; external QtIntf name 'QFont_create4';
procedure QFont_family(handle: QFontH; retval: PWideString); cdecl; external QtIntf name 'QFont_family';
procedure QFont_setFamily(handle: QFontH; p1: PWideString); cdecl; external QtIntf name 'QFont_setFamily';
function QFont_pointSize(handle: QFontH): Integer; cdecl; external QtIntf name 'QFont_pointSize';
procedure QFont_setPointSize(handle: QFontH; p1: Integer); cdecl; external QtIntf name 'QFont_setPointSize';
function QFont_pointSizeF(handle: QFontH): Double; cdecl; external QtIntf name 'QFont_pointSizeF';
procedure QFont_setPointSizeF(handle: QFontH; p1: Double); cdecl; external QtIntf name 'QFont_setPointSizeF';
function QFont_pixelSize(handle: QFontH): Integer; cdecl; external QtIntf name 'QFont_pixelSize';
procedure QFont_setPixelSize(handle: QFontH; p1: Integer); cdecl; external QtIntf name 'QFont_setPixelSize';
function QFont_weight(handle: QFontH): Integer; cdecl; external QtIntf name 'QFont_weight';
procedure QFont_setWeight(handle: QFontH; p1: Integer); cdecl; external QtIntf name 'QFont_setWeight';
function QFont_bold(handle: QFontH): Boolean; cdecl; external QtIntf name 'QFont_bold';
procedure QFont_setBold(handle: QFontH; p1: Boolean); cdecl; external QtIntf name 'QFont_setBold';
procedure QFont_setStyle(handle: QFontH; style: QFontStyle); cdecl; external QtIntf name 'QFont_setStyle';
function QFont_style(handle: QFontH): QFontStyle; cdecl; external QtIntf name 'QFont_style';
function QFont_italic(handle: QFontH): Boolean; cdecl; external QtIntf name 'QFont_italic';
procedure QFont_setItalic(handle: QFontH; b: Boolean); cdecl; external QtIntf name 'QFont_setItalic';
function QFont_underline(handle: QFontH): Boolean; cdecl; external QtIntf name 'QFont_underline';
procedure QFont_setUnderline(handle: QFontH; p1: Boolean); cdecl; external QtIntf name 'QFont_setUnderline';
function QFont_overline(handle: QFontH): Boolean; cdecl; external QtIntf name 'QFont_overline';
procedure QFont_setOverline(handle: QFontH; p1: Boolean); cdecl; external QtIntf name 'QFont_setOverline';
function QFont_strikeOut(handle: QFontH): Boolean; cdecl; external QtIntf name 'QFont_strikeOut';
procedure QFont_setStrikeOut(handle: QFontH; p1: Boolean); cdecl; external QtIntf name 'QFont_setStrikeOut';
function QFont_fixedPitch(handle: QFontH): Boolean; cdecl; external QtIntf name 'QFont_fixedPitch';
procedure QFont_setFixedPitch(handle: QFontH; p1: Boolean); cdecl; external QtIntf name 'QFont_setFixedPitch';
function QFont_kerning(handle: QFontH): Boolean; cdecl; external QtIntf name 'QFont_kerning';
procedure QFont_setKerning(handle: QFontH; p1: Boolean); cdecl; external QtIntf name 'QFont_setKerning';
function QFont_styleHint(handle: QFontH): QFontStyleHint; cdecl; external QtIntf name 'QFont_styleHint';
function QFont_styleStrategy(handle: QFontH): QFontStyleStrategy; cdecl; external QtIntf name 'QFont_styleStrategy';
procedure QFont_setStyleHint(handle: QFontH; p1: QFontStyleHint; p2: QFontStyleStrategy = QFontPreferDefault); cdecl; external QtIntf name 'QFont_setStyleHint';
procedure QFont_setStyleStrategy(handle: QFontH; s: QFontStyleStrategy); cdecl; external QtIntf name 'QFont_setStyleStrategy';
function QFont_stretch(handle: QFontH): Integer; cdecl; external QtIntf name 'QFont_stretch';
procedure QFont_setStretch(handle: QFontH; p1: Integer); cdecl; external QtIntf name 'QFont_setStretch';
function QFont_rawMode(handle: QFontH): Boolean; cdecl; external QtIntf name 'QFont_rawMode';
procedure QFont_setRawMode(handle: QFontH; p1: Boolean); cdecl; external QtIntf name 'QFont_setRawMode';
function QFont_exactMatch(handle: QFontH): Boolean; cdecl; external QtIntf name 'QFont_exactMatch';
function QFont_isCopyOf(handle: QFontH; p1: QFontH): Boolean; cdecl; external QtIntf name 'QFont_isCopyOf';
{$ifdef UNIX or DARWIN }
function QFont_handle(handle: QFontH): QtHANDLE; overload; cdecl; external QtIntf name 'QFont_handle';
{$endif}
procedure QFont_setRawName(handle: QFontH; p1: PWideString); cdecl; external QtIntf name 'QFont_setRawName';
procedure QFont_rawName(handle: QFontH; retval: PWideString); cdecl; external QtIntf name 'QFont_rawName';
procedure QFont_key(handle: QFontH; retval: PWideString); cdecl; external QtIntf name 'QFont_key';
procedure QFont_toString(handle: QFontH; retval: PWideString); cdecl; external QtIntf name 'QFont_toString';
function QFont_fromString(handle: QFontH; p1: PWideString): Boolean; cdecl; external QtIntf name 'QFont_fromString';
procedure QFont_substitute(retval: PWideString; p1: PWideString); cdecl; external QtIntf name 'QFont_substitute';
procedure QFont_substitutes(retval: QStringListH; p1: PWideString); cdecl; external QtIntf name 'QFont_substitutes';
procedure QFont_substitutions(retval: QStringListH); cdecl; external QtIntf name 'QFont_substitutions';
procedure QFont_insertSubstitution(p1: PWideString; p2: PWideString); cdecl; external QtIntf name 'QFont_insertSubstitution';
procedure QFont_insertSubstitutions(p1: PWideString; p2: QStringListH); cdecl; external QtIntf name 'QFont_insertSubstitutions';
procedure QFont_removeSubstitution(p1: PWideString); cdecl; external QtIntf name 'QFont_removeSubstitution';
procedure QFont_initialize(); cdecl; external QtIntf name 'QFont_initialize';
procedure QFont_cleanup(); cdecl; external QtIntf name 'QFont_cleanup';
procedure QFont_cacheStatistics(); cdecl; external QtIntf name 'QFont_cacheStatistics';
procedure QFont_defaultFamily(handle: QFontH; retval: PWideString); cdecl; external QtIntf name 'QFont_defaultFamily';
procedure QFont_lastResortFamily(handle: QFontH; retval: PWideString); cdecl; external QtIntf name 'QFont_lastResortFamily';
procedure QFont_lastResortFont(handle: QFontH; retval: PWideString); cdecl; external QtIntf name 'QFont_lastResortFont';
procedure QFont_resolve(handle: QFontH; retval: QFontH; p1: QFontH); overload; cdecl; external QtIntf name 'QFont_resolve';
function QFont_resolve(handle: QFontH): LongWord; overload; cdecl; external QtIntf name 'QFont_resolve2';
procedure QFont_resolve(handle: QFontH; mask: LongWord); overload; cdecl; external QtIntf name 'QFont_resolve3';
{$ifdef MSWINDOWS }
function QFont_handle(handle: QFontH): HFONT; overload; cdecl; external QtIntf name 'QFont_handle2';
{$endif}

type
  QFontDatabaseWritingSystem = cardinal; //  QFontDatabase::WritingSystem (4)

const
    QFontDatabaseAny = 0 { $0 };
    QFontDatabaseLatin = 1 { $1 };
    QFontDatabaseGreek = 2 { $2 };
    QFontDatabaseCyrillic = 3 { $3 };
    QFontDatabaseArmenian = 4 { $4 };
    QFontDatabaseHebrew = 5 { $5 };
    QFontDatabaseArabic = 6 { $6 };
    QFontDatabaseSyriac = 7 { $7 };
    QFontDatabaseThaana = 8 { $8 };
    QFontDatabaseDevanagari = 9 { $9 };
    QFontDatabaseBengali = 10 { $a };
    QFontDatabaseGurmukhi = 11 { $b };
    QFontDatabaseGujarati = 12 { $c };
    QFontDatabaseOriya = 13 { $d };
    QFontDatabaseTamil = 14 { $e };
    QFontDatabaseTelugu = 15 { $f };
    QFontDatabaseKannada = 16 { $10 };
    QFontDatabaseMalayalam = 17 { $11 };
    QFontDatabaseSinhala = 18 { $12 };
    QFontDatabaseThai = 19 { $13 };
    QFontDatabaseLao = 20 { $14 };
    QFontDatabaseTibetan = 21 { $15 };
    QFontDatabaseMyanmar = 22 { $16 };
    QFontDatabaseGeorgian = 23 { $17 };
    QFontDatabaseKhmer = 24 { $18 };
    QFontDatabaseSimplifiedChinese = 25 { $19 };
    QFontDatabaseTraditionalChinese = 26 { $1a };
    QFontDatabaseJapanese = 27 { $1b };
    QFontDatabaseKorean = 28 { $1c };
    QFontDatabaseVietnamese = 29 { $1d };
    QFontDatabaseSymbol = 30 { $1e };
    QFontDatabaseOther = 30 { $1e };
    QFontDatabaseOgham = 31 { $1f };
    QFontDatabaseRunic = 32 { $20 };
    QFontDatabaseWritingSystemsCount = 33 { $21 };


procedure QFontDatabase_standardSizes(retval: PIntArray); cdecl; external QtIntf name 'QFontDatabase_standardSizes';
function QFontDatabase_create(): QFontDatabaseH; cdecl; external QtIntf name 'QFontDatabase_create';
procedure QFontDatabase_destroy(handle: QFontDatabaseH); cdecl; external QtIntf name 'QFontDatabase_destroy'; 
procedure QFontDatabase_writingSystems(handle: QFontDatabaseH; retval: PIntArray); overload; cdecl; external QtIntf name 'QFontDatabase_writingSystems';
procedure QFontDatabase_writingSystems(handle: QFontDatabaseH; retval: PIntArray; family: PWideString); overload; cdecl; external QtIntf name 'QFontDatabase_writingSystems2';
procedure QFontDatabase_families(handle: QFontDatabaseH; retval: QStringListH; writingSystem: QFontDatabaseWritingSystem = QFontDatabaseAny); cdecl; external QtIntf name 'QFontDatabase_families';
procedure QFontDatabase_styles(handle: QFontDatabaseH; retval: QStringListH; family: PWideString); cdecl; external QtIntf name 'QFontDatabase_styles';
procedure QFontDatabase_pointSizes(handle: QFontDatabaseH; retval: PIntArray; family: PWideString; style: PWideString = nil); cdecl; external QtIntf name 'QFontDatabase_pointSizes';
procedure QFontDatabase_smoothSizes(handle: QFontDatabaseH; retval: PIntArray; family: PWideString; style: PWideString); cdecl; external QtIntf name 'QFontDatabase_smoothSizes';
procedure QFontDatabase_styleString(handle: QFontDatabaseH; retval: PWideString; font: QFontH); overload; cdecl; external QtIntf name 'QFontDatabase_styleString';
procedure QFontDatabase_styleString(handle: QFontDatabaseH; retval: PWideString; fontInfo: QFontInfoH); overload; cdecl; external QtIntf name 'QFontDatabase_styleString2';
procedure QFontDatabase_font(handle: QFontDatabaseH; retval: QFontH; family: PWideString; style: PWideString; pointSize: Integer); cdecl; external QtIntf name 'QFontDatabase_font';
function QFontDatabase_isBitmapScalable(handle: QFontDatabaseH; family: PWideString; style: PWideString = nil): Boolean; cdecl; external QtIntf name 'QFontDatabase_isBitmapScalable';
function QFontDatabase_isSmoothlyScalable(handle: QFontDatabaseH; family: PWideString; style: PWideString = nil): Boolean; cdecl; external QtIntf name 'QFontDatabase_isSmoothlyScalable';
function QFontDatabase_isScalable(handle: QFontDatabaseH; family: PWideString; style: PWideString = nil): Boolean; cdecl; external QtIntf name 'QFontDatabase_isScalable';
function QFontDatabase_isFixedPitch(handle: QFontDatabaseH; family: PWideString; style: PWideString = nil): Boolean; cdecl; external QtIntf name 'QFontDatabase_isFixedPitch';
function QFontDatabase_italic(handle: QFontDatabaseH; family: PWideString; style: PWideString): Boolean; cdecl; external QtIntf name 'QFontDatabase_italic';
function QFontDatabase_bold(handle: QFontDatabaseH; family: PWideString; style: PWideString): Boolean; cdecl; external QtIntf name 'QFontDatabase_bold';
function QFontDatabase_weight(handle: QFontDatabaseH; family: PWideString; style: PWideString): Integer; cdecl; external QtIntf name 'QFontDatabase_weight';
procedure QFontDatabase_writingSystemName(retval: PWideString; writingSystem: QFontDatabaseWritingSystem); cdecl; external QtIntf name 'QFontDatabase_writingSystemName';
procedure QFontDatabase_writingSystemSample(retval: PWideString; writingSystem: QFontDatabaseWritingSystem); cdecl; external QtIntf name 'QFontDatabase_writingSystemSample';
function QFontDatabase_addApplicationFont(fileName: PWideString): Integer; cdecl; external QtIntf name 'QFontDatabase_addApplicationFont';
function QFontDatabase_addApplicationFontFromData(fontData: QByteArrayH): Integer; cdecl; external QtIntf name 'QFontDatabase_addApplicationFontFromData';
procedure QFontDatabase_applicationFontFamilies(retval: QStringListH; id: Integer); cdecl; external QtIntf name 'QFontDatabase_applicationFontFamilies';
function QFontDatabase_removeApplicationFont(id: Integer): Boolean; cdecl; external QtIntf name 'QFontDatabase_removeApplicationFont';
function QFontDatabase_removeAllApplicationFonts(): Boolean; cdecl; external QtIntf name 'QFontDatabase_removeAllApplicationFonts';


type
  QTextCursorMoveMode = ( // QTextCursor::MoveMode (1)
    QTextCursorMoveAnchor, QTextCursorKeepAnchor );

  QTextCursorMoveOperation = ( // QTextCursor::MoveOperation (1)
    QTextCursorNoMove, QTextCursorStart, QTextCursorUp, QTextCursorStartOfLine, QTextCursorStartOfBlock, QTextCursorStartOfWord, QTextCursorPreviousBlock, QTextCursorPreviousCharacter, 
    QTextCursorPreviousWord, QTextCursorLeft, QTextCursorWordLeft, QTextCursorEnd, QTextCursorDown, QTextCursorEndOfLine, QTextCursorEndOfWord, QTextCursorEndOfBlock, QTextCursorNextBlock, 
    QTextCursorNextCharacter, QTextCursorNextWord, QTextCursorRight, QTextCursorWordRight );

  QTextCursorSelectionType = ( // QTextCursor::SelectionType (1)
    QTextCursorWordUnderCursor, QTextCursorLineUnderCursor, QTextCursorBlockUnderCursor, QTextCursorDocument );

function QTextCursor_create(): QTextCursorH; overload; cdecl; external QtIntf name 'QTextCursor_create';
procedure QTextCursor_destroy(handle: QTextCursorH); cdecl; external QtIntf name 'QTextCursor_destroy'; 
function QTextCursor_create(document: QTextDocumentH): QTextCursorH; overload; cdecl; external QtIntf name 'QTextCursor_create2';
function QTextCursor_create(frame: QTextFrameH): QTextCursorH; overload; cdecl; external QtIntf name 'QTextCursor_create3';
function QTextCursor_create(block: QTextBlockH): QTextCursorH; overload; cdecl; external QtIntf name 'QTextCursor_create4';
function QTextCursor_create(cursor: QTextCursorH): QTextCursorH; overload; cdecl; external QtIntf name 'QTextCursor_create6';
function QTextCursor_isNull(handle: QTextCursorH): Boolean; cdecl; external QtIntf name 'QTextCursor_isNull';
procedure QTextCursor_setPosition(handle: QTextCursorH; pos: Integer; mode: QTextCursorMoveMode = QTextCursorMoveAnchor); cdecl; external QtIntf name 'QTextCursor_setPosition';
function QTextCursor_position(handle: QTextCursorH): Integer; cdecl; external QtIntf name 'QTextCursor_position';
function QTextCursor_anchor(handle: QTextCursorH): Integer; cdecl; external QtIntf name 'QTextCursor_anchor';
procedure QTextCursor_insertText(handle: QTextCursorH; text: PWideString); overload; cdecl; external QtIntf name 'QTextCursor_insertText';
procedure QTextCursor_insertText(handle: QTextCursorH; text: PWideString; format: QTextCharFormatH); overload; cdecl; external QtIntf name 'QTextCursor_insertText2';
function QTextCursor_movePosition(handle: QTextCursorH; op: QTextCursorMoveOperation; p2: QTextCursorMoveMode = QTextCursorMoveAnchor; n: Integer = 1): Boolean; cdecl; external QtIntf name 'QTextCursor_movePosition';
procedure QTextCursor_deleteChar(handle: QTextCursorH); cdecl; external QtIntf name 'QTextCursor_deleteChar';
procedure QTextCursor_deletePreviousChar(handle: QTextCursorH); cdecl; external QtIntf name 'QTextCursor_deletePreviousChar';
procedure QTextCursor_select(handle: QTextCursorH; selection: QTextCursorSelectionType); cdecl; external QtIntf name 'QTextCursor_select';
function QTextCursor_hasSelection(handle: QTextCursorH): Boolean; cdecl; external QtIntf name 'QTextCursor_hasSelection';
function QTextCursor_hasComplexSelection(handle: QTextCursorH): Boolean; cdecl; external QtIntf name 'QTextCursor_hasComplexSelection';
procedure QTextCursor_removeSelectedText(handle: QTextCursorH); cdecl; external QtIntf name 'QTextCursor_removeSelectedText';
procedure QTextCursor_clearSelection(handle: QTextCursorH); cdecl; external QtIntf name 'QTextCursor_clearSelection';
function QTextCursor_selectionStart(handle: QTextCursorH): Integer; cdecl; external QtIntf name 'QTextCursor_selectionStart';
function QTextCursor_selectionEnd(handle: QTextCursorH): Integer; cdecl; external QtIntf name 'QTextCursor_selectionEnd';
procedure QTextCursor_selectedText(handle: QTextCursorH; retval: PWideString); cdecl; external QtIntf name 'QTextCursor_selectedText';
procedure QTextCursor_selection(handle: QTextCursorH; retval: QTextDocumentFragmentH); cdecl; external QtIntf name 'QTextCursor_selection';
procedure QTextCursor_selectedTableCells(handle: QTextCursorH; firstRow: PInteger; numRows: PInteger; firstColumn: PInteger; numColumns: PInteger); cdecl; external QtIntf name 'QTextCursor_selectedTableCells';
procedure QTextCursor_block(handle: QTextCursorH; retval: QTextBlockH); cdecl; external QtIntf name 'QTextCursor_block';
procedure QTextCursor_charFormat(handle: QTextCursorH; retval: QTextCharFormatH); cdecl; external QtIntf name 'QTextCursor_charFormat';
procedure QTextCursor_setCharFormat(handle: QTextCursorH; format: QTextCharFormatH); cdecl; external QtIntf name 'QTextCursor_setCharFormat';
procedure QTextCursor_mergeCharFormat(handle: QTextCursorH; modifier: QTextCharFormatH); cdecl; external QtIntf name 'QTextCursor_mergeCharFormat';
procedure QTextCursor_blockFormat(handle: QTextCursorH; retval: QTextBlockFormatH); cdecl; external QtIntf name 'QTextCursor_blockFormat';
procedure QTextCursor_setBlockFormat(handle: QTextCursorH; format: QTextBlockFormatH); cdecl; external QtIntf name 'QTextCursor_setBlockFormat';
procedure QTextCursor_mergeBlockFormat(handle: QTextCursorH; modifier: QTextBlockFormatH); cdecl; external QtIntf name 'QTextCursor_mergeBlockFormat';
procedure QTextCursor_blockCharFormat(handle: QTextCursorH; retval: QTextCharFormatH); cdecl; external QtIntf name 'QTextCursor_blockCharFormat';
procedure QTextCursor_setBlockCharFormat(handle: QTextCursorH; format: QTextCharFormatH); cdecl; external QtIntf name 'QTextCursor_setBlockCharFormat';
procedure QTextCursor_mergeBlockCharFormat(handle: QTextCursorH; modifier: QTextCharFormatH); cdecl; external QtIntf name 'QTextCursor_mergeBlockCharFormat';
function QTextCursor_atBlockStart(handle: QTextCursorH): Boolean; cdecl; external QtIntf name 'QTextCursor_atBlockStart';
function QTextCursor_atBlockEnd(handle: QTextCursorH): Boolean; cdecl; external QtIntf name 'QTextCursor_atBlockEnd';
function QTextCursor_atStart(handle: QTextCursorH): Boolean; cdecl; external QtIntf name 'QTextCursor_atStart';
function QTextCursor_atEnd(handle: QTextCursorH): Boolean; cdecl; external QtIntf name 'QTextCursor_atEnd';
procedure QTextCursor_insertBlock(handle: QTextCursorH); overload; cdecl; external QtIntf name 'QTextCursor_insertBlock';
procedure QTextCursor_insertBlock(handle: QTextCursorH; format: QTextBlockFormatH); overload; cdecl; external QtIntf name 'QTextCursor_insertBlock2';
procedure QTextCursor_insertBlock(handle: QTextCursorH; format: QTextBlockFormatH; charFormat: QTextCharFormatH); overload; cdecl; external QtIntf name 'QTextCursor_insertBlock3';
function QTextCursor_currentList(handle: QTextCursorH): QTextListH; cdecl; external QtIntf name 'QTextCursor_currentList';
function QTextCursor_insertTable(handle: QTextCursorH; rows: Integer; cols: Integer; format: QTextTableFormatH): QTextTableH; overload; cdecl; external QtIntf name 'QTextCursor_insertTable';
function QTextCursor_insertTable(handle: QTextCursorH; rows: Integer; cols: Integer): QTextTableH; overload; cdecl; external QtIntf name 'QTextCursor_insertTable2';
function QTextCursor_currentTable(handle: QTextCursorH): QTextTableH; cdecl; external QtIntf name 'QTextCursor_currentTable';
function QTextCursor_insertFrame(handle: QTextCursorH; format: QTextFrameFormatH): QTextFrameH; cdecl; external QtIntf name 'QTextCursor_insertFrame';
function QTextCursor_currentFrame(handle: QTextCursorH): QTextFrameH; cdecl; external QtIntf name 'QTextCursor_currentFrame';
procedure QTextCursor_insertFragment(handle: QTextCursorH; fragment: QTextDocumentFragmentH); cdecl; external QtIntf name 'QTextCursor_insertFragment';
procedure QTextCursor_insertHtml(handle: QTextCursorH; html: PWideString); cdecl; external QtIntf name 'QTextCursor_insertHtml';
procedure QTextCursor_insertImage(handle: QTextCursorH; format: QTextImageFormatH); overload; cdecl; external QtIntf name 'QTextCursor_insertImage2';
procedure QTextCursor_insertImage(handle: QTextCursorH; name: PWideString); overload; cdecl; external QtIntf name 'QTextCursor_insertImage3';
procedure QTextCursor_beginEditBlock(handle: QTextCursorH); cdecl; external QtIntf name 'QTextCursor_beginEditBlock';
procedure QTextCursor_joinPreviousEditBlock(handle: QTextCursorH); cdecl; external QtIntf name 'QTextCursor_joinPreviousEditBlock';
procedure QTextCursor_endEditBlock(handle: QTextCursorH); cdecl; external QtIntf name 'QTextCursor_endEditBlock';
function QTextCursor_isCopyOf(handle: QTextCursorH; other: QTextCursorH): Boolean; cdecl; external QtIntf name 'QTextCursor_isCopyOf';
function QTextCursor_blockNumber(handle: QTextCursorH): Integer; cdecl; external QtIntf name 'QTextCursor_blockNumber';
function QTextCursor_columnNumber(handle: QTextCursorH): Integer; cdecl; external QtIntf name 'QTextCursor_columnNumber';


type
  QTextOptionWrapMode = ( // QTextOption::WrapMode (1)
    QTextOptionNoWrap, QTextOptionWordWrap, QTextOptionManualWrap, QTextOptionWrapAnywhere, QTextOptionWrapAtWordBoundaryOrAnywhere );

type
  QTextOptionFlag = cardinal; // QTextOption::Flag
  QTextOptionFlags = QTextOptionFlag; //QFlags<> (3)
const
  QTextOptionIncludeTrailingSpaces =   $80000000;

function QTextOption_create(): QTextOptionH; overload; cdecl; external QtIntf name 'QTextOption_create';
procedure QTextOption_destroy(handle: QTextOptionH); cdecl; external QtIntf name 'QTextOption_destroy'; 
function QTextOption_create(alignment: QtAlignment): QTextOptionH; overload; cdecl; external QtIntf name 'QTextOption_create2';
function QTextOption_create(o: QTextOptionH): QTextOptionH; overload; cdecl; external QtIntf name 'QTextOption_create3';
procedure QTextOption_setAlignment(handle: QTextOptionH; alignment: QtAlignment); cdecl; external QtIntf name 'QTextOption_setAlignment';
function QTextOption_alignment(handle: QTextOptionH): QtAlignment; cdecl; external QtIntf name 'QTextOption_alignment';
procedure QTextOption_setTextDirection(handle: QTextOptionH; aDirection: QtLayoutDirection); cdecl; external QtIntf name 'QTextOption_setTextDirection';
function QTextOption_textDirection(handle: QTextOptionH): QtLayoutDirection; cdecl; external QtIntf name 'QTextOption_textDirection';
procedure QTextOption_setWrapMode(handle: QTextOptionH; wrap: QTextOptionWrapMode); cdecl; external QtIntf name 'QTextOption_setWrapMode';
function QTextOption_wrapMode(handle: QTextOptionH): QTextOptionWrapMode; cdecl; external QtIntf name 'QTextOption_wrapMode';
procedure QTextOption_setFlags(handle: QTextOptionH; flags: QTextOptionFlags); cdecl; external QtIntf name 'QTextOption_setFlags';
function QTextOption_flags(handle: QTextOptionH): QTextOptionFlags; cdecl; external QtIntf name 'QTextOption_flags';
procedure QTextOption_setTabStop(handle: QTextOptionH; tabStop: Double); cdecl; external QtIntf name 'QTextOption_setTabStop';
function QTextOption_tabStop(handle: QTextOptionH): Double; cdecl; external QtIntf name 'QTextOption_tabStop';
procedure QTextOption_setTabArray(handle: QTextOptionH; tabStops: PIntArray); cdecl; external QtIntf name 'QTextOption_setTabArray';
procedure QTextOption_tabArray(handle: QTextOptionH; retval: PIntArray); cdecl; external QtIntf name 'QTextOption_tabArray';
procedure QTextOption_setUseDesignMetrics(handle: QTextOptionH; b: Boolean); cdecl; external QtIntf name 'QTextOption_setUseDesignMetrics';
function QTextOption_useDesignMetrics(handle: QTextOptionH): Boolean; cdecl; external QtIntf name 'QTextOption_useDesignMetrics';

function QFontMetrics_create(p1: QFontH): QFontMetricsH; overload; cdecl; external QtIntf name 'QFontMetrics_create';
procedure QFontMetrics_destroy(handle: QFontMetricsH); cdecl; external QtIntf name 'QFontMetrics_destroy'; 
function QFontMetrics_create(p1: QFontH; pd: QPaintDeviceH): QFontMetricsH; overload; cdecl; external QtIntf name 'QFontMetrics_create2';
function QFontMetrics_create(p1: QFontMetricsH): QFontMetricsH; overload; cdecl; external QtIntf name 'QFontMetrics_create3';
function QFontMetrics_ascent(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_ascent';
function QFontMetrics_descent(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_descent';
function QFontMetrics_height(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_height';
function QFontMetrics_leading(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_leading';
function QFontMetrics_lineSpacing(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_lineSpacing';
function QFontMetrics_minLeftBearing(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_minLeftBearing';
function QFontMetrics_minRightBearing(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_minRightBearing';
function QFontMetrics_maxWidth(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_maxWidth';
function QFontMetrics_xHeight(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_xHeight';
function QFontMetrics_averageCharWidth(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_averageCharWidth';
function QFontMetrics_inFont(handle: QFontMetricsH; p1: PWideChar): Boolean; cdecl; external QtIntf name 'QFontMetrics_inFont';
function QFontMetrics_leftBearing(handle: QFontMetricsH; p1: PWideChar): Integer; cdecl; external QtIntf name 'QFontMetrics_leftBearing';
function QFontMetrics_rightBearing(handle: QFontMetricsH; p1: PWideChar): Integer; cdecl; external QtIntf name 'QFontMetrics_rightBearing';
function QFontMetrics_width(handle: QFontMetricsH; p1: PWideString; len: Integer = -1): Integer; overload; cdecl; external QtIntf name 'QFontMetrics_width';
function QFontMetrics_width(handle: QFontMetricsH; p1: PWideChar): Integer; overload; cdecl; external QtIntf name 'QFontMetrics_width2';
function QFontMetrics_charWidth(handle: QFontMetricsH; str: PWideString; pos: Integer): Integer; cdecl; external QtIntf name 'QFontMetrics_charWidth';
procedure QFontMetrics_boundingRect(handle: QFontMetricsH; retval: PRect; p1: PWideChar); overload; cdecl; external QtIntf name 'QFontMetrics_boundingRect';
procedure QFontMetrics_boundingRect(handle: QFontMetricsH; retval: PRect; text: PWideString); overload; cdecl; external QtIntf name 'QFontMetrics_boundingRect2';
procedure QFontMetrics_boundingRect(handle: QFontMetricsH; retval: PRect; r: PRect; flags: Integer; text: PWideString; tabstops: Integer = 0; tabarray: PInteger = nil); overload; cdecl; external QtIntf name 'QFontMetrics_boundingRect3';
procedure QFontMetrics_boundingRect(handle: QFontMetricsH; retval: PRect; x: Integer; y: Integer; w: Integer; h: Integer; flags: Integer; text: PWideString; tabstops: Integer = 0; tabarray: PInteger = nil); overload; cdecl; external QtIntf name 'QFontMetrics_boundingRect4';
procedure QFontMetrics_size(handle: QFontMetricsH; retval: PSize; flags: Integer; str: PWideString; tabstops: Integer = 0; tabarray: PInteger = nil); cdecl; external QtIntf name 'QFontMetrics_size';
procedure QFontMetrics_elidedText(handle: QFontMetricsH; retval: PWideString; text: PWideString; mode: QtTextElideMode; width: Integer; flags: Integer = 0); cdecl; external QtIntf name 'QFontMetrics_elidedText';
function QFontMetrics_underlinePos(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_underlinePos';
function QFontMetrics_overlinePos(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_overlinePos';
function QFontMetrics_strikeOutPos(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_strikeOutPos';
function QFontMetrics_lineWidth(handle: QFontMetricsH): Integer; cdecl; external QtIntf name 'QFontMetrics_lineWidth';

function QFontMetricsF_create(p1: QFontH): QFontMetricsFH; overload; cdecl; external QtIntf name 'QFontMetricsF_create';
procedure QFontMetricsF_destroy(handle: QFontMetricsFH); cdecl; external QtIntf name 'QFontMetricsF_destroy'; 
function QFontMetricsF_create(p1: QFontH; pd: QPaintDeviceH): QFontMetricsFH; overload; cdecl; external QtIntf name 'QFontMetricsF_create2';
function QFontMetricsF_create(p1: QFontMetricsFH): QFontMetricsFH; overload; cdecl; external QtIntf name 'QFontMetricsF_create4';
function QFontMetricsF_ascent(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_ascent';
function QFontMetricsF_descent(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_descent';
function QFontMetricsF_height(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_height';
function QFontMetricsF_leading(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_leading';
function QFontMetricsF_lineSpacing(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_lineSpacing';
function QFontMetricsF_minLeftBearing(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_minLeftBearing';
function QFontMetricsF_minRightBearing(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_minRightBearing';
function QFontMetricsF_maxWidth(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_maxWidth';
function QFontMetricsF_xHeight(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_xHeight';
function QFontMetricsF_averageCharWidth(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_averageCharWidth';
function QFontMetricsF_inFont(handle: QFontMetricsFH; p1: PWideChar): Boolean; cdecl; external QtIntf name 'QFontMetricsF_inFont';
function QFontMetricsF_leftBearing(handle: QFontMetricsFH; p1: PWideChar): Double; cdecl; external QtIntf name 'QFontMetricsF_leftBearing';
function QFontMetricsF_rightBearing(handle: QFontMetricsFH; p1: PWideChar): Double; cdecl; external QtIntf name 'QFontMetricsF_rightBearing';
function QFontMetricsF_width(handle: QFontMetricsFH; _string: PWideString): Double; overload; cdecl; external QtIntf name 'QFontMetricsF_width';
function QFontMetricsF_width(handle: QFontMetricsFH; p1: PWideChar): Double; overload; cdecl; external QtIntf name 'QFontMetricsF_width2';
procedure QFontMetricsF_boundingRect(handle: QFontMetricsFH; retval: QRectFH; _string: PWideString); overload; cdecl; external QtIntf name 'QFontMetricsF_boundingRect';
procedure QFontMetricsF_boundingRect(handle: QFontMetricsFH; retval: QRectFH; p1: PWideChar); overload; cdecl; external QtIntf name 'QFontMetricsF_boundingRect2';
procedure QFontMetricsF_boundingRect(handle: QFontMetricsFH; retval: QRectFH; r: QRectFH; flags: Integer; _string: PWideString; tabstops: Integer = 0; tabarray: PInteger = nil); overload; cdecl; external QtIntf name 'QFontMetricsF_boundingRect3';
procedure QFontMetricsF_size(handle: QFontMetricsFH; retval: QSizeFH; flags: Integer; str: PWideString; tabstops: Integer = 0; tabarray: PInteger = nil); cdecl; external QtIntf name 'QFontMetricsF_size';
procedure QFontMetricsF_elidedText(handle: QFontMetricsFH; retval: PWideString; text: PWideString; mode: QtTextElideMode; width: Double; flags: Integer = 0); cdecl; external QtIntf name 'QFontMetricsF_elidedText';
function QFontMetricsF_underlinePos(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_underlinePos';
function QFontMetricsF_overlinePos(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_overlinePos';
function QFontMetricsF_strikeOutPos(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_strikeOutPos';
function QFontMetricsF_lineWidth(handle: QFontMetricsFH): Double; cdecl; external QtIntf name 'QFontMetricsF_lineWidth';

function QFontInfo_create(p1: QFontH): QFontInfoH; overload; cdecl; external QtIntf name 'QFontInfo_create';
procedure QFontInfo_destroy(handle: QFontInfoH); cdecl; external QtIntf name 'QFontInfo_destroy'; 
function QFontInfo_create(p1: QFontInfoH): QFontInfoH; overload; cdecl; external QtIntf name 'QFontInfo_create2';
procedure QFontInfo_family(handle: QFontInfoH; retval: PWideString); cdecl; external QtIntf name 'QFontInfo_family';
function QFontInfo_pixelSize(handle: QFontInfoH): Integer; cdecl; external QtIntf name 'QFontInfo_pixelSize';
function QFontInfo_pointSize(handle: QFontInfoH): Integer; cdecl; external QtIntf name 'QFontInfo_pointSize';
function QFontInfo_pointSizeF(handle: QFontInfoH): Double; cdecl; external QtIntf name 'QFontInfo_pointSizeF';
function QFontInfo_italic(handle: QFontInfoH): Boolean; cdecl; external QtIntf name 'QFontInfo_italic';
function QFontInfo_style(handle: QFontInfoH): QFontStyle; cdecl; external QtIntf name 'QFontInfo_style';
function QFontInfo_weight(handle: QFontInfoH): Integer; cdecl; external QtIntf name 'QFontInfo_weight';
function QFontInfo_bold(handle: QFontInfoH): Boolean; cdecl; external QtIntf name 'QFontInfo_bold';
function QFontInfo_underline(handle: QFontInfoH): Boolean; cdecl; external QtIntf name 'QFontInfo_underline';
function QFontInfo_overline(handle: QFontInfoH): Boolean; cdecl; external QtIntf name 'QFontInfo_overline';
function QFontInfo_strikeOut(handle: QFontInfoH): Boolean; cdecl; external QtIntf name 'QFontInfo_strikeOut';
function QFontInfo_fixedPitch(handle: QFontInfoH): Boolean; cdecl; external QtIntf name 'QFontInfo_fixedPitch';
function QFontInfo_styleHint(handle: QFontInfoH): QFontStyleHint; cdecl; external QtIntf name 'QFontInfo_styleHint';
function QFontInfo_rawMode(handle: QFontInfoH): Boolean; cdecl; external QtIntf name 'QFontInfo_rawMode';
function QFontInfo_exactMatch(handle: QFontInfoH): Boolean; cdecl; external QtIntf name 'QFontInfo_exactMatch';


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
    QTextDocumentStyleSheetResource = 3,
    QTextDocumentUserResource = 100 );

function QTextDocument_create(parent: QObjectH = nil): QTextDocumentH; overload; cdecl; external QtIntf name 'QTextDocument_create';
procedure QTextDocument_destroy(handle: QTextDocumentH); cdecl; external QtIntf name 'QTextDocument_destroy'; 
function QTextDocument_create(text: PWideString; parent: QObjectH = nil): QTextDocumentH; overload; cdecl; external QtIntf name 'QTextDocument_create2';
function QTextDocument_clone(handle: QTextDocumentH; parent: QObjectH = nil): QTextDocumentH; cdecl; external QtIntf name 'QTextDocument_clone';
function QTextDocument_isEmpty(handle: QTextDocumentH): Boolean; cdecl; external QtIntf name 'QTextDocument_isEmpty';
procedure QTextDocument_clear(handle: QTextDocumentH); cdecl; external QtIntf name 'QTextDocument_clear';
procedure QTextDocument_setUndoRedoEnabled(handle: QTextDocumentH; enable: Boolean); cdecl; external QtIntf name 'QTextDocument_setUndoRedoEnabled';
function QTextDocument_isUndoRedoEnabled(handle: QTextDocumentH): Boolean; cdecl; external QtIntf name 'QTextDocument_isUndoRedoEnabled';
function QTextDocument_isUndoAvailable(handle: QTextDocumentH): Boolean; cdecl; external QtIntf name 'QTextDocument_isUndoAvailable';
function QTextDocument_isRedoAvailable(handle: QTextDocumentH): Boolean; cdecl; external QtIntf name 'QTextDocument_isRedoAvailable';
procedure QTextDocument_setDocumentLayout(handle: QTextDocumentH; layout: QAbstractTextDocumentLayoutH); cdecl; external QtIntf name 'QTextDocument_setDocumentLayout';
function QTextDocument_documentLayout(handle: QTextDocumentH): QAbstractTextDocumentLayoutH; cdecl; external QtIntf name 'QTextDocument_documentLayout';
procedure QTextDocument_setMetaInformation(handle: QTextDocumentH; info: QTextDocumentMetaInformation; p2: PWideString); cdecl; external QtIntf name 'QTextDocument_setMetaInformation';
procedure QTextDocument_metaInformation(handle: QTextDocumentH; retval: PWideString; info: QTextDocumentMetaInformation); cdecl; external QtIntf name 'QTextDocument_metaInformation';
procedure QTextDocument_toHtml(handle: QTextDocumentH; retval: PWideString; encoding: QByteArrayH = nil); cdecl; external QtIntf name 'QTextDocument_toHtml';
procedure QTextDocument_setHtml(handle: QTextDocumentH; html: PWideString); cdecl; external QtIntf name 'QTextDocument_setHtml';
procedure QTextDocument_toPlainText(handle: QTextDocumentH; retval: PWideString); cdecl; external QtIntf name 'QTextDocument_toPlainText';
procedure QTextDocument_setPlainText(handle: QTextDocumentH; text: PWideString); cdecl; external QtIntf name 'QTextDocument_setPlainText';
procedure QTextDocument_find(handle: QTextDocumentH; retval: QTextCursorH; subString: PWideString; from: Integer = 0; options: QTextDocumentFindFlags = 0); overload; cdecl; external QtIntf name 'QTextDocument_find';
procedure QTextDocument_find(handle: QTextDocumentH; retval: QTextCursorH; subString: PWideString; from: QTextCursorH; options: QTextDocumentFindFlags = 0); overload; cdecl; external QtIntf name 'QTextDocument_find2';
procedure QTextDocument_find(handle: QTextDocumentH; retval: QTextCursorH; expr: QRegExpH; from: Integer = 0; options: QTextDocumentFindFlags = 0); overload; cdecl; external QtIntf name 'QTextDocument_find3';
procedure QTextDocument_find(handle: QTextDocumentH; retval: QTextCursorH; expr: QRegExpH; from: QTextCursorH; options: QTextDocumentFindFlags = 0); overload; cdecl; external QtIntf name 'QTextDocument_find4';
function QTextDocument_frameAt(handle: QTextDocumentH; pos: Integer): QTextFrameH; cdecl; external QtIntf name 'QTextDocument_frameAt';
function QTextDocument_rootFrame(handle: QTextDocumentH): QTextFrameH; cdecl; external QtIntf name 'QTextDocument_rootFrame';
function QTextDocument_object(handle: QTextDocumentH; objectIndex: Integer): QTextObjectH; cdecl; external QtIntf name 'QTextDocument_object';
function QTextDocument_objectForFormat(handle: QTextDocumentH; p1: QTextFormatH): QTextObjectH; cdecl; external QtIntf name 'QTextDocument_objectForFormat';
procedure QTextDocument_findBlock(handle: QTextDocumentH; retval: QTextBlockH; pos: Integer); cdecl; external QtIntf name 'QTextDocument_findBlock';
procedure QTextDocument_begin(handle: QTextDocumentH; retval: QTextBlockH); cdecl; external QtIntf name 'QTextDocument_begin';
procedure QTextDocument_end(handle: QTextDocumentH; retval: QTextBlockH); cdecl; external QtIntf name 'QTextDocument_end';
procedure QTextDocument_setPageSize(handle: QTextDocumentH; size: QSizeFH); cdecl; external QtIntf name 'QTextDocument_setPageSize';
procedure QTextDocument_pageSize(handle: QTextDocumentH; retval: QSizeFH); cdecl; external QtIntf name 'QTextDocument_pageSize';
procedure QTextDocument_setDefaultFont(handle: QTextDocumentH; font: QFontH); cdecl; external QtIntf name 'QTextDocument_setDefaultFont';
procedure QTextDocument_defaultFont(handle: QTextDocumentH; retval: QFontH); cdecl; external QtIntf name 'QTextDocument_defaultFont';
function QTextDocument_pageCount(handle: QTextDocumentH): Integer; cdecl; external QtIntf name 'QTextDocument_pageCount';
function QTextDocument_isModified(handle: QTextDocumentH): Boolean; cdecl; external QtIntf name 'QTextDocument_isModified';
procedure QTextDocument_print(handle: QTextDocumentH; printer: QPrinterH); cdecl; external QtIntf name 'QTextDocument_print';
procedure QTextDocument_resource(handle: QTextDocumentH; retval: QVariantH; _type: Integer; name: QUrlH); cdecl; external QtIntf name 'QTextDocument_resource';
procedure QTextDocument_addResource(handle: QTextDocumentH; _type: Integer; name: QUrlH; resource: QVariantH); cdecl; external QtIntf name 'QTextDocument_addResource';
procedure QTextDocument_markContentsDirty(handle: QTextDocumentH; from: Integer; length: Integer); cdecl; external QtIntf name 'QTextDocument_markContentsDirty';
procedure QTextDocument_setUseDesignMetrics(handle: QTextDocumentH; b: Boolean); cdecl; external QtIntf name 'QTextDocument_setUseDesignMetrics';
function QTextDocument_useDesignMetrics(handle: QTextDocumentH): Boolean; cdecl; external QtIntf name 'QTextDocument_useDesignMetrics';
procedure QTextDocument_drawContents(handle: QTextDocumentH; painter: QPainterH; rect: QRectFH = nil); cdecl; external QtIntf name 'QTextDocument_drawContents';
procedure QTextDocument_setTextWidth(handle: QTextDocumentH; width: Double); cdecl; external QtIntf name 'QTextDocument_setTextWidth';
function QTextDocument_textWidth(handle: QTextDocumentH): Double; cdecl; external QtIntf name 'QTextDocument_textWidth';
function QTextDocument_idealWidth(handle: QTextDocumentH): Double; cdecl; external QtIntf name 'QTextDocument_idealWidth';
procedure QTextDocument_adjustSize(handle: QTextDocumentH); cdecl; external QtIntf name 'QTextDocument_adjustSize';
procedure QTextDocument_size(handle: QTextDocumentH; retval: QSizeFH); cdecl; external QtIntf name 'QTextDocument_size';
function QTextDocument_blockCount(handle: QTextDocumentH): Integer; cdecl; external QtIntf name 'QTextDocument_blockCount';
procedure QTextDocument_setDefaultStyleSheet(handle: QTextDocumentH; sheet: PWideString); cdecl; external QtIntf name 'QTextDocument_setDefaultStyleSheet';
procedure QTextDocument_defaultStyleSheet(handle: QTextDocumentH; retval: PWideString); cdecl; external QtIntf name 'QTextDocument_defaultStyleSheet';
procedure QTextDocument_undo(handle: QTextDocumentH; cursor: QTextCursorH); overload; cdecl; external QtIntf name 'QTextDocument_undo';
procedure QTextDocument_redo(handle: QTextDocumentH; cursor: QTextCursorH); overload; cdecl; external QtIntf name 'QTextDocument_redo';
function QTextDocument_maximumBlockCount(handle: QTextDocumentH): Integer; cdecl; external QtIntf name 'QTextDocument_maximumBlockCount';
procedure QTextDocument_setMaximumBlockCount(handle: QTextDocumentH; maximum: Integer); cdecl; external QtIntf name 'QTextDocument_setMaximumBlockCount';
procedure QTextDocument_undo(handle: QTextDocumentH); overload; cdecl; external QtIntf name 'QTextDocument_undo2';
procedure QTextDocument_redo(handle: QTextDocumentH); overload; cdecl; external QtIntf name 'QTextDocument_redo2';
procedure QTextDocument_setModified(handle: QTextDocumentH; m: Boolean = True); cdecl; external QtIntf name 'QTextDocument_setModified';


type
  QIconMode = ( // QIcon::Mode (1)
    QIconNormal, QIconDisabled, QIconActive, QIconSelected );

  QIconState = ( // QIcon::State (1)
    QIconOn, QIconOff );

function QIcon_create(): QIconH; overload; cdecl; external QtIntf name 'QIcon_create';
procedure QIcon_destroy(handle: QIconH); cdecl; external QtIntf name 'QIcon_destroy'; 
function QIcon_create(pixmap: QPixmapH): QIconH; overload; cdecl; external QtIntf name 'QIcon_create2';
function QIcon_create(other: QIconH): QIconH; overload; cdecl; external QtIntf name 'QIcon_create3';
function QIcon_create(fileName: PWideString): QIconH; overload; cdecl; external QtIntf name 'QIcon_create4';
function QIcon_create(engine: QIconEngineH): QIconH; overload; cdecl; external QtIntf name 'QIcon_create5';
procedure QIcon_pixmap(handle: QIconH; retval: QPixmapH; size: PSize; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); overload; cdecl; external QtIntf name 'QIcon_pixmap';
procedure QIcon_pixmap(handle: QIconH; retval: QPixmapH; w: Integer; h: Integer; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); overload; cdecl; external QtIntf name 'QIcon_pixmap2';
procedure QIcon_pixmap(handle: QIconH; retval: QPixmapH; extent: Integer; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); overload; cdecl; external QtIntf name 'QIcon_pixmap3';
procedure QIcon_actualSize(handle: QIconH; retval: PSize; size: PSize; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); cdecl; external QtIntf name 'QIcon_actualSize';
procedure QIcon_paint(handle: QIconH; painter: QPainterH; rect: PRect; alignment: QtAlignment = QtAlignCenter; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); overload; cdecl; external QtIntf name 'QIcon_paint';
procedure QIcon_paint(handle: QIconH; painter: QPainterH; x: Integer; y: Integer; w: Integer; h: Integer; alignment: QtAlignment = QtAlignCenter; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); overload; cdecl; external QtIntf name 'QIcon_paint2';
function QIcon_isNull(handle: QIconH): Boolean; cdecl; external QtIntf name 'QIcon_isNull';
function QIcon_isDetached(handle: QIconH): Boolean; cdecl; external QtIntf name 'QIcon_isDetached';
function QIcon_serialNumber(handle: QIconH): Integer; cdecl; external QtIntf name 'QIcon_serialNumber';
procedure QIcon_addPixmap(handle: QIconH; pixmap: QPixmapH; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); cdecl; external QtIntf name 'QIcon_addPixmap';
procedure QIcon_addFile(handle: QIconH; fileName: PWideString; size: PSize = nil; mode: QIconMode = QIconNormal; state: QIconState = QIconOff); cdecl; external QtIntf name 'QIcon_addFile';

{$ifdef MSWINDOWS }

type
  QPixmapHBitmapFormat = ( // QPixmap::HBitmapFormat (1)
    QPixmapNoAlpha, QPixmapPremultipliedAlpha );

{$endif}
function QPixmap_create(): QPixmapH; overload; cdecl; external QtIntf name 'QPixmap_create';
procedure QPixmap_destroy(handle: QPixmapH); cdecl; external QtIntf name 'QPixmap_destroy'; 
function QPixmap_create(w: Integer; h: Integer): QPixmapH; overload; cdecl; external QtIntf name 'QPixmap_create2';
function QPixmap_create(p1: PSize): QPixmapH; overload; cdecl; external QtIntf name 'QPixmap_create3';
function QPixmap_create(fileName: PWideString; format: PAnsiChar = nil; flags: QtImageConversionFlags = QtAutoColor): QPixmapH; overload; cdecl; external QtIntf name 'QPixmap_create4';
function QPixmap_create(xpm: PAnsiChar): QPixmapH; overload; cdecl; external QtIntf name 'QPixmap_create5';
function QPixmap_create(p1: QPixmapH): QPixmapH; overload; cdecl; external QtIntf name 'QPixmap_create6';
function QPixmap_isNull(handle: QPixmapH): Boolean; cdecl; external QtIntf name 'QPixmap_isNull';
function QPixmap_devType(handle: QPixmapH): Integer; cdecl; external QtIntf name 'QPixmap_devType';
function QPixmap_width(handle: QPixmapH): Integer; cdecl; external QtIntf name 'QPixmap_width';
function QPixmap_height(handle: QPixmapH): Integer; cdecl; external QtIntf name 'QPixmap_height';
procedure QPixmap_size(handle: QPixmapH; retval: PSize); cdecl; external QtIntf name 'QPixmap_size';
procedure QPixmap_rect(handle: QPixmapH; retval: PRect); cdecl; external QtIntf name 'QPixmap_rect';
function QPixmap_depth(handle: QPixmapH): Integer; cdecl; external QtIntf name 'QPixmap_depth';
function QPixmap_defaultDepth(): Integer; cdecl; external QtIntf name 'QPixmap_defaultDepth';
procedure QPixmap_fill(handle: QPixmapH; fillColor: PQColor); overload; cdecl; external QtIntf name 'QPixmap_fill';
procedure QPixmap_fill(handle: QPixmapH; widget: QWidgetH; ofs: PQtPoint); overload; cdecl; external QtIntf name 'QPixmap_fill2';
procedure QPixmap_fill(handle: QPixmapH; widget: QWidgetH; xofs: Integer; yofs: Integer); overload; cdecl; external QtIntf name 'QPixmap_fill3';
procedure QPixmap_mask(handle: QPixmapH; retval: QBitmapH); cdecl; external QtIntf name 'QPixmap_mask';
procedure QPixmap_setMask(handle: QPixmapH; p1: QBitmapH); cdecl; external QtIntf name 'QPixmap_setMask';
procedure QPixmap_alphaChannel(handle: QPixmapH; retval: QPixmapH); cdecl; external QtIntf name 'QPixmap_alphaChannel';
procedure QPixmap_setAlphaChannel(handle: QPixmapH; p1: QPixmapH); cdecl; external QtIntf name 'QPixmap_setAlphaChannel';
function QPixmap_hasAlpha(handle: QPixmapH): Boolean; cdecl; external QtIntf name 'QPixmap_hasAlpha';
function QPixmap_hasAlphaChannel(handle: QPixmapH): Boolean; cdecl; external QtIntf name 'QPixmap_hasAlphaChannel';
procedure QPixmap_createHeuristicMask(handle: QPixmapH; retval: QBitmapH; clipTight: Boolean = True); cdecl; external QtIntf name 'QPixmap_createHeuristicMask';
procedure QPixmap_createMaskFromColor(handle: QPixmapH; retval: QBitmapH; maskColor: PQColor); cdecl; external QtIntf name 'QPixmap_createMaskFromColor';
procedure QPixmap_grabWindow(retval: QPixmapH; p1: LongWord; x: Integer = 0; y: Integer = 0; w: Integer = -1; h: Integer = -1); cdecl; external QtIntf name 'QPixmap_grabWindow';
procedure QPixmap_grabWidget(retval: QPixmapH; widget: QWidgetH; rect: PRect); overload; cdecl; external QtIntf name 'QPixmap_grabWidget';
procedure QPixmap_grabWidget(retval: QPixmapH; widget: QWidgetH; x: Integer = 0; y: Integer = 0; w: Integer = -1; h: Integer = -1); overload; cdecl; external QtIntf name 'QPixmap_grabWidget2';
procedure QPixmap_scaled(handle: QPixmapH; retval: QPixmapH; w: Integer; h: Integer; aspectMode: QtAspectRatioMode = QtIgnoreAspectRatio; mode: QtTransformationMode = QtFastTransformation); overload; cdecl; external QtIntf name 'QPixmap_scaled';
procedure QPixmap_scaled(handle: QPixmapH; retval: QPixmapH; s: PSize; aspectMode: QtAspectRatioMode = QtIgnoreAspectRatio; mode: QtTransformationMode = QtFastTransformation); overload; cdecl; external QtIntf name 'QPixmap_scaled2';
procedure QPixmap_scaledToWidth(handle: QPixmapH; retval: QPixmapH; w: Integer; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtIntf name 'QPixmap_scaledToWidth';
procedure QPixmap_scaledToHeight(handle: QPixmapH; retval: QPixmapH; h: Integer; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtIntf name 'QPixmap_scaledToHeight';
procedure QPixmap_transformed(handle: QPixmapH; retval: QPixmapH; p1: QMatrixH; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtIntf name 'QPixmap_transformed';
procedure QPixmap_trueMatrix(retval: QMatrixH; m: QMatrixH; w: Integer; h: Integer); cdecl; external QtIntf name 'QPixmap_trueMatrix';
procedure QPixmap_toImage(handle: QPixmapH; retval: QImageH); cdecl; external QtIntf name 'QPixmap_toImage';
procedure QPixmap_fromImage(retval: QPixmapH; image: QImageH; flags: QtImageConversionFlags = QtAutoColor); cdecl; external QtIntf name 'QPixmap_fromImage';
function QPixmap_load(handle: QPixmapH; fileName: PWideString; format: PAnsiChar = nil; flags: QtImageConversionFlags = QtAutoColor): Boolean; cdecl; external QtIntf name 'QPixmap_load';
function QPixmap_loadFromData(handle: QPixmapH; buf: PByte; len: LongWord; format: PAnsiChar = nil; flags: QtImageConversionFlags = QtAutoColor): Boolean; overload; cdecl; external QtIntf name 'QPixmap_loadFromData';
function QPixmap_loadFromData(handle: QPixmapH; data: QByteArrayH; format: PAnsiChar = nil; flags: QtImageConversionFlags = QtAutoColor): Boolean; overload; cdecl; external QtIntf name 'QPixmap_loadFromData2';
function QPixmap_save(handle: QPixmapH; fileName: PWideString; format: PAnsiChar = nil; quality: Integer = -1): Boolean; overload; cdecl; external QtIntf name 'QPixmap_save';
function QPixmap_save(handle: QPixmapH; device: QIODeviceH; format: PAnsiChar = nil; quality: Integer = -1): Boolean; overload; cdecl; external QtIntf name 'QPixmap_save2';
procedure QPixmap_copy(handle: QPixmapH; retval: QPixmapH; x: Integer; y: Integer; width: Integer; height: Integer); overload; cdecl; external QtIntf name 'QPixmap_copy';
procedure QPixmap_copy(handle: QPixmapH; retval: QPixmapH; rect: PRect = nil); overload; cdecl; external QtIntf name 'QPixmap_copy2';
function QPixmap_serialNumber(handle: QPixmapH): Integer; cdecl; external QtIntf name 'QPixmap_serialNumber';
function QPixmap_isDetached(handle: QPixmapH): Boolean; cdecl; external QtIntf name 'QPixmap_isDetached';
procedure QPixmap_detach(handle: QPixmapH); cdecl; external QtIntf name 'QPixmap_detach';
function QPixmap_isQBitmap(handle: QPixmapH): Boolean; cdecl; external QtIntf name 'QPixmap_isQBitmap';
{$ifdef UNIX }
function QPixmap_x11SetDefaultScreen(screen: Integer): Integer; cdecl; external QtIntf name 'QPixmap_x11SetDefaultScreen';
procedure QPixmap_x11SetScreen(handle: QPixmapH; screen: Integer); cdecl; external QtIntf name 'QPixmap_x11SetScreen';
function QPixmap_x11Info(handle: QPixmapH): QX11InfoH; cdecl; external QtIntf name 'QPixmap_x11Info';
function QPixmap_x11PictureHandle(handle: QPixmapH): QtHANDLE; cdecl; external QtIntf name 'QPixmap_x11PictureHandle';
function QPixmap_handle(handle: QPixmapH): QtHANDLE; cdecl; external QtIntf name 'QPixmap_handle';
{$endif}
function QPixmap_paintEngine(handle: QPixmapH): QPaintEngineH; cdecl; external QtIntf name 'QPixmap_paintEngine';
{$ifdef MSWINDOWS }
function QPixmap_toWinHBITMAP(handle: QPixmapH; format: QPixmapHBitmapFormat = QPixmapNoAlpha): HBITMAP; cdecl; external QtIntf name 'QPixmap_toWinHBITMAP';
procedure QPixmap_fromWinHBITMAP(retval: QPixmapH; hbitmap: HBITMAP; format: QPixmapHBitmapFormat = QPixmapNoAlpha); cdecl; external QtIntf name 'QPixmap_fromWinHBITMAP';
{$endif}
{$ifdef DARWIN }
function QPixmap_toMacCGImageRef(handle: QPixmapH): CGImageRef; cdecl; external QtIntf name 'QPixmap_toMacCGImageRef';
procedure QPixmap_fromMacCGImageRef(retval: QPixmapH; image: CGImageRef); cdecl; external QtIntf name 'QPixmap_fromMacCGImageRef';
function QPixmap_macQDHandle(handle: QPixmapH): QtHANDLE; cdecl; external QtIntf name 'QPixmap_macQDHandle';
function QPixmap_macQDAlphaHandle(handle: QPixmapH): QtHANDLE; cdecl; external QtIntf name 'QPixmap_macQDAlphaHandle';
function QPixmap_macCGHandle(handle: QPixmapH): QtHANDLE; cdecl; external QtIntf name 'QPixmap_macCGHandle';
{$endif}


type
  QImageInvertMode = ( // QImage::InvertMode (1)
    QImageInvertRgb, QImageInvertRgba );

  QImageFormat = ( // QImage::Format (1)
    QImageFormat_Invalid, QImageFormat_Mono, QImageFormat_MonoLSB, QImageFormat_Indexed8, QImageFormat_RGB32, QImageFormat_ARGB32, QImageFormat_ARGB32_Premultiplied, QImageFormat_RGB16, 
    QImageNImageFormats );

function QImage_create(): QImageH; overload; cdecl; external QtIntf name 'QImage_create';
procedure QImage_destroy(handle: QImageH); cdecl; external QtIntf name 'QImage_destroy'; 
function QImage_create(size: PSize; format: QImageFormat): QImageH; overload; cdecl; external QtIntf name 'QImage_create2';
function QImage_create(width: Integer; height: Integer; format: QImageFormat): QImageH; overload; cdecl; external QtIntf name 'QImage_create3';
function QImage_create(data: PByte; width: Integer; height: Integer; format: QImageFormat): QImageH; overload; cdecl; external QtIntf name 'QImage_create4';
function QImage_create(xpm: PAnsiChar): QImageH; overload; cdecl; external QtIntf name 'QImage_create6';
function QImage_create(fileName: PAnsiChar; format: PAnsiChar = nil): QImageH; overload; cdecl; external QtIntf name 'QImage_create8';
function QImage_create(p1: QImageH): QImageH; overload; cdecl; external QtIntf name 'QImage_create9';
function QImage_isNull(handle: QImageH): Boolean; cdecl; external QtIntf name 'QImage_isNull';
function QImage_devType(handle: QImageH): Integer; cdecl; external QtIntf name 'QImage_devType';
procedure QImage_detach(handle: QImageH); cdecl; external QtIntf name 'QImage_detach';
function QImage_isDetached(handle: QImageH): Boolean; cdecl; external QtIntf name 'QImage_isDetached';
procedure QImage_copy(handle: QImageH; retval: QImageH; rect: PRect = nil); overload; cdecl; external QtIntf name 'QImage_copy';
procedure QImage_copy(handle: QImageH; retval: QImageH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QImage_copy2';
function QImage_format(handle: QImageH): QImageFormat; cdecl; external QtIntf name 'QImage_format';
procedure QImage_convertToFormat(handle: QImageH; retval: QImageH; f: QImageFormat; flags: QtImageConversionFlags = QtAutoColor); overload; cdecl; external QtIntf name 'QImage_convertToFormat';
function QImage_width(handle: QImageH): Integer; cdecl; external QtIntf name 'QImage_width';
function QImage_height(handle: QImageH): Integer; cdecl; external QtIntf name 'QImage_height';
procedure QImage_size(handle: QImageH; retval: PSize); cdecl; external QtIntf name 'QImage_size';
procedure QImage_rect(handle: QImageH; retval: PRect); cdecl; external QtIntf name 'QImage_rect';
function QImage_depth(handle: QImageH): Integer; cdecl; external QtIntf name 'QImage_depth';
function QImage_numColors(handle: QImageH): Integer; cdecl; external QtIntf name 'QImage_numColors';
function QImage_color(handle: QImageH; i: Integer): QRgb; cdecl; external QtIntf name 'QImage_color';
procedure QImage_setColor(handle: QImageH; i: Integer; c: QRgb); cdecl; external QtIntf name 'QImage_setColor';
procedure QImage_setNumColors(handle: QImageH; p1: Integer); cdecl; external QtIntf name 'QImage_setNumColors';
function QImage_allGray(handle: QImageH): Boolean; cdecl; external QtIntf name 'QImage_allGray';
function QImage_isGrayscale(handle: QImageH): Boolean; cdecl; external QtIntf name 'QImage_isGrayscale';
function QImage_bits(handle: QImageH): PByte; overload; cdecl; external QtIntf name 'QImage_bits';
function QImage_numBytes(handle: QImageH): Integer; cdecl; external QtIntf name 'QImage_numBytes';
function QImage_scanLine(handle: QImageH; p1: Integer): PByte; overload; cdecl; external QtIntf name 'QImage_scanLine';
function QImage_bytesPerLine(handle: QImageH): Integer; cdecl; external QtIntf name 'QImage_bytesPerLine';
function QImage_valid(handle: QImageH; x: Integer; y: Integer): Boolean; overload; cdecl; external QtIntf name 'QImage_valid';
function QImage_valid(handle: QImageH; pt: PQtPoint): Boolean; overload; cdecl; external QtIntf name 'QImage_valid2';
function QImage_pixelIndex(handle: QImageH; x: Integer; y: Integer): Integer; overload; cdecl; external QtIntf name 'QImage_pixelIndex';
function QImage_pixelIndex(handle: QImageH; pt: PQtPoint): Integer; overload; cdecl; external QtIntf name 'QImage_pixelIndex2';
function QImage_pixel(handle: QImageH; x: Integer; y: Integer): QRgb; overload; cdecl; external QtIntf name 'QImage_pixel';
function QImage_pixel(handle: QImageH; pt: PQtPoint): QRgb; overload; cdecl; external QtIntf name 'QImage_pixel2';
procedure QImage_setPixel(handle: QImageH; x: Integer; y: Integer; index_or_rgb: LongWord); overload; cdecl; external QtIntf name 'QImage_setPixel';
procedure QImage_setPixel(handle: QImageH; pt: PQtPoint; index_or_rgb: LongWord); overload; cdecl; external QtIntf name 'QImage_setPixel2';
procedure QImage_fill(handle: QImageH; pixel: LongWord); cdecl; external QtIntf name 'QImage_fill';
function QImage_hasAlphaChannel(handle: QImageH): Boolean; cdecl; external QtIntf name 'QImage_hasAlphaChannel';
procedure QImage_setAlphaChannel(handle: QImageH; alphaChannel: QImageH); cdecl; external QtIntf name 'QImage_setAlphaChannel';
procedure QImage_alphaChannel(handle: QImageH; retval: QImageH); cdecl; external QtIntf name 'QImage_alphaChannel';
procedure QImage_createAlphaMask(handle: QImageH; retval: QImageH; flags: QtImageConversionFlags = QtAutoColor); cdecl; external QtIntf name 'QImage_createAlphaMask';
procedure QImage_createHeuristicMask(handle: QImageH; retval: QImageH; clipTight: Boolean = True); cdecl; external QtIntf name 'QImage_createHeuristicMask';
procedure QImage_scaled(handle: QImageH; retval: QImageH; w: Integer; h: Integer; aspectMode: QtAspectRatioMode = QtIgnoreAspectRatio; mode: QtTransformationMode = QtFastTransformation); overload; cdecl; external QtIntf name 'QImage_scaled';
procedure QImage_scaled(handle: QImageH; retval: QImageH; s: PSize; aspectMode: QtAspectRatioMode = QtIgnoreAspectRatio; mode: QtTransformationMode = QtFastTransformation); overload; cdecl; external QtIntf name 'QImage_scaled2';
procedure QImage_scaledToWidth(handle: QImageH; retval: QImageH; w: Integer; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtIntf name 'QImage_scaledToWidth';
procedure QImage_scaledToHeight(handle: QImageH; retval: QImageH; h: Integer; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtIntf name 'QImage_scaledToHeight';
procedure QImage_transformed(handle: QImageH; retval: QImageH; matrix: QMatrixH; mode: QtTransformationMode = QtFastTransformation); cdecl; external QtIntf name 'QImage_transformed';
procedure QImage_trueMatrix(retval: QMatrixH; p1: QMatrixH; w: Integer; h: Integer); cdecl; external QtIntf name 'QImage_trueMatrix';
procedure QImage_mirrored(handle: QImageH; retval: QImageH; horizontally: Boolean = False; vertically: Boolean = True); cdecl; external QtIntf name 'QImage_mirrored';
procedure QImage_rgbSwapped(handle: QImageH; retval: QImageH); cdecl; external QtIntf name 'QImage_rgbSwapped';
procedure QImage_invertPixels(handle: QImageH; p1: QImageInvertMode = QImageInvertRgb); cdecl; external QtIntf name 'QImage_invertPixels';
function QImage_load(handle: QImageH; device: QIODeviceH; format: PAnsiChar): Boolean; overload; cdecl; external QtIntf name 'QImage_load';
function QImage_load(handle: QImageH; fileName: PWideString; format: PAnsiChar = nil): Boolean; overload; cdecl; external QtIntf name 'QImage_load2';
function QImage_loadFromData(handle: QImageH; buf: PByte; len: Integer; format: PAnsiChar = nil): Boolean; overload; cdecl; external QtIntf name 'QImage_loadFromData';
function QImage_loadFromData(handle: QImageH; data: QByteArrayH; aformat: PAnsiChar = nil): Boolean; overload; cdecl; external QtIntf name 'QImage_loadFromData2';
function QImage_save(handle: QImageH; fileName: PWideString; format: PAnsiChar = nil; quality: Integer = -1): Boolean; overload; cdecl; external QtIntf name 'QImage_save';
function QImage_save(handle: QImageH; device: QIODeviceH; format: PAnsiChar = nil; quality: Integer = -1): Boolean; overload; cdecl; external QtIntf name 'QImage_save2';
procedure QImage_fromData(retval: QImageH; data: PByte; size: Integer; format: PAnsiChar = nil); overload; cdecl; external QtIntf name 'QImage_fromData';
procedure QImage_fromData(retval: QImageH; data: QByteArrayH; format: PAnsiChar = nil); overload; cdecl; external QtIntf name 'QImage_fromData2';
function QImage_serialNumber(handle: QImageH): Integer; cdecl; external QtIntf name 'QImage_serialNumber';
function QImage_paintEngine(handle: QImageH): QPaintEngineH; cdecl; external QtIntf name 'QImage_paintEngine';
function QImage_dotsPerMeterX(handle: QImageH): Integer; cdecl; external QtIntf name 'QImage_dotsPerMeterX';
function QImage_dotsPerMeterY(handle: QImageH): Integer; cdecl; external QtIntf name 'QImage_dotsPerMeterY';
procedure QImage_setDotsPerMeterX(handle: QImageH; p1: Integer); cdecl; external QtIntf name 'QImage_setDotsPerMeterX';
procedure QImage_setDotsPerMeterY(handle: QImageH; p1: Integer); cdecl; external QtIntf name 'QImage_setDotsPerMeterY';
procedure QImage_offset(handle: QImageH; retval: PQtPoint); cdecl; external QtIntf name 'QImage_offset';
procedure QImage_setOffset(handle: QImageH; p1: PQtPoint); cdecl; external QtIntf name 'QImage_setOffset';
procedure QImage_textKeys(handle: QImageH; retval: QStringListH); cdecl; external QtIntf name 'QImage_textKeys';
procedure QImage_text(handle: QImageH; retval: PWideString; key: PWideString = nil); overload; cdecl; external QtIntf name 'QImage_text';
procedure QImage_setText(handle: QImageH; key: PWideString; value: PWideString); overload; cdecl; external QtIntf name 'QImage_setText';
procedure QImage_text(handle: QImageH; retval: PWideString; key: PAnsiChar; lang: PAnsiChar = nil); overload; cdecl; external QtIntf name 'QImage_text2';
procedure QImage_textLanguages(handle: QImageH; retval: QStringListH); cdecl; external QtIntf name 'QImage_textLanguages';
procedure QImage_setText(handle: QImageH; key: PAnsiChar; lang: PAnsiChar; p3: PWideString); overload; cdecl; external QtIntf name 'QImage_setText2';

function QBitmap_create(): QBitmapH; overload; cdecl; external QtIntf name 'QBitmap_create';
procedure QBitmap_destroy(handle: QBitmapH); cdecl; external QtIntf name 'QBitmap_destroy'; 
function QBitmap_create(p1: QPixmapH): QBitmapH; overload; cdecl; external QtIntf name 'QBitmap_create2';
function QBitmap_create(w: Integer; h: Integer): QBitmapH; overload; cdecl; external QtIntf name 'QBitmap_create3';
function QBitmap_create(p1: PSize): QBitmapH; overload; cdecl; external QtIntf name 'QBitmap_create4';
function QBitmap_create(fileName: PWideString; format: PAnsiChar = nil): QBitmapH; overload; cdecl; external QtIntf name 'QBitmap_create5';
procedure QBitmap_clear(handle: QBitmapH); cdecl; external QtIntf name 'QBitmap_clear';
procedure QBitmap_fromImage(retval: QBitmapH; image: QImageH; flags: QtImageConversionFlags = QtAutoColor); cdecl; external QtIntf name 'QBitmap_fromImage';
procedure QBitmap_fromData(retval: QBitmapH; size: PSize; bits: PByte; monoFormat: QImageFormat = QImageFormat_MonoLSB); cdecl; external QtIntf name 'QBitmap_fromData';
procedure QBitmap_transformed(handle: QBitmapH; retval: QBitmapH; p1: QMatrixH); cdecl; external QtIntf name 'QBitmap_transformed';

function QPicture_create(formatVersion: Integer = -1): QPictureH; overload; cdecl; external QtIntf name 'QPicture_create';
procedure QPicture_destroy(handle: QPictureH); cdecl; external QtIntf name 'QPicture_destroy'; 
function QPicture_create(p1: QPictureH): QPictureH; overload; cdecl; external QtIntf name 'QPicture_create2';
function QPicture_isNull(handle: QPictureH): Boolean; cdecl; external QtIntf name 'QPicture_isNull';
function QPicture_devType(handle: QPictureH): Integer; cdecl; external QtIntf name 'QPicture_devType';
function QPicture_size(handle: QPictureH): LongWord; cdecl; external QtIntf name 'QPicture_size';
function QPicture_data(handle: QPictureH): PAnsiChar; cdecl; external QtIntf name 'QPicture_data';
procedure QPicture_setData(handle: QPictureH; data: PAnsiChar; size: LongWord); cdecl; external QtIntf name 'QPicture_setData';
function QPicture_play(handle: QPictureH; p: QPainterH): Boolean; cdecl; external QtIntf name 'QPicture_play';
function QPicture_load(handle: QPictureH; dev: QIODeviceH; format: PAnsiChar = nil): Boolean; overload; cdecl; external QtIntf name 'QPicture_load';
function QPicture_load(handle: QPictureH; fileName: PWideString; format: PAnsiChar = nil): Boolean; overload; cdecl; external QtIntf name 'QPicture_load2';
function QPicture_save(handle: QPictureH; dev: QIODeviceH; format: PAnsiChar = nil): Boolean; overload; cdecl; external QtIntf name 'QPicture_save';
function QPicture_save(handle: QPictureH; fileName: PWideString; format: PAnsiChar = nil): Boolean; overload; cdecl; external QtIntf name 'QPicture_save2';
procedure QPicture_boundingRect(handle: QPictureH; retval: PRect); cdecl; external QtIntf name 'QPicture_boundingRect';
procedure QPicture_setBoundingRect(handle: QPictureH; r: PRect); cdecl; external QtIntf name 'QPicture_setBoundingRect';
procedure QPicture_detach(handle: QPictureH); cdecl; external QtIntf name 'QPicture_detach';
function QPicture_isDetached(handle: QPictureH): Boolean; cdecl; external QtIntf name 'QPicture_isDetached';
function QPicture_pictureFormat(fileName: PWideString): PAnsiChar; cdecl; external QtIntf name 'QPicture_pictureFormat';
procedure QPicture_inputFormatList(retval: QStringListH); cdecl; external QtIntf name 'QPicture_inputFormatList';
procedure QPicture_outputFormatList(retval: QStringListH); cdecl; external QtIntf name 'QPicture_outputFormatList';
function QPicture_paintEngine(handle: QPictureH): QPaintEngineH; cdecl; external QtIntf name 'QPicture_paintEngine';

function QPictureIO_create(): QPictureIOH; overload; cdecl; external QtIntf name 'QPictureIO_create';
procedure QPictureIO_destroy(handle: QPictureIOH); cdecl; external QtIntf name 'QPictureIO_destroy'; 
function QPictureIO_create(ioDevice: QIODeviceH; format: PAnsiChar): QPictureIOH; overload; cdecl; external QtIntf name 'QPictureIO_create2';
function QPictureIO_create(fileName: PWideString; format: PAnsiChar): QPictureIOH; overload; cdecl; external QtIntf name 'QPictureIO_create3';
function QPictureIO_picture(handle: QPictureIOH): QPictureH; cdecl; external QtIntf name 'QPictureIO_picture';
function QPictureIO_status(handle: QPictureIOH): Integer; cdecl; external QtIntf name 'QPictureIO_status';
function QPictureIO_format(handle: QPictureIOH): PAnsiChar; cdecl; external QtIntf name 'QPictureIO_format';
function QPictureIO_ioDevice(handle: QPictureIOH): QIODeviceH; cdecl; external QtIntf name 'QPictureIO_ioDevice';
procedure QPictureIO_fileName(handle: QPictureIOH; retval: PWideString); cdecl; external QtIntf name 'QPictureIO_fileName';
function QPictureIO_quality(handle: QPictureIOH): Integer; cdecl; external QtIntf name 'QPictureIO_quality';
procedure QPictureIO_description(handle: QPictureIOH; retval: PWideString); cdecl; external QtIntf name 'QPictureIO_description';
function QPictureIO_parameters(handle: QPictureIOH): PAnsiChar; cdecl; external QtIntf name 'QPictureIO_parameters';
function QPictureIO_gamma(handle: QPictureIOH): Single; cdecl; external QtIntf name 'QPictureIO_gamma';
procedure QPictureIO_setPicture(handle: QPictureIOH; p1: QPictureH); cdecl; external QtIntf name 'QPictureIO_setPicture';
procedure QPictureIO_setStatus(handle: QPictureIOH; p1: Integer); cdecl; external QtIntf name 'QPictureIO_setStatus';
procedure QPictureIO_setFormat(handle: QPictureIOH; p1: PAnsiChar); cdecl; external QtIntf name 'QPictureIO_setFormat';
procedure QPictureIO_setIODevice(handle: QPictureIOH; p1: QIODeviceH); cdecl; external QtIntf name 'QPictureIO_setIODevice';
procedure QPictureIO_setFileName(handle: QPictureIOH; p1: PWideString); cdecl; external QtIntf name 'QPictureIO_setFileName';
procedure QPictureIO_setQuality(handle: QPictureIOH; p1: Integer); cdecl; external QtIntf name 'QPictureIO_setQuality';
procedure QPictureIO_setDescription(handle: QPictureIOH; p1: PWideString); cdecl; external QtIntf name 'QPictureIO_setDescription';
procedure QPictureIO_setParameters(handle: QPictureIOH; p1: PAnsiChar); cdecl; external QtIntf name 'QPictureIO_setParameters';
procedure QPictureIO_setGamma(handle: QPictureIOH; p1: Single); cdecl; external QtIntf name 'QPictureIO_setGamma';
function QPictureIO_read(handle: QPictureIOH): Boolean; cdecl; external QtIntf name 'QPictureIO_read';
function QPictureIO_write(handle: QPictureIOH): Boolean; cdecl; external QtIntf name 'QPictureIO_write';
procedure QPictureIO_pictureFormat(retval: QByteArrayH; fileName: PWideString); overload; cdecl; external QtIntf name 'QPictureIO_pictureFormat';
procedure QPictureIO_pictureFormat(retval: QByteArrayH; p1: QIODeviceH); overload; cdecl; external QtIntf name 'QPictureIO_pictureFormat2';
procedure QPictureIO_defineIOHandler(format: PAnsiChar; header: PAnsiChar; flags: PAnsiChar; read_picture: TPictureIOHandler; write_picture: TPictureIOHandler); cdecl; external QtIntf name 'QPictureIO_defineIOHandler';


type
  QImageIOHandlerImageOption = ( // QImageIOHandler::ImageOption (1)
    QImageIOHandlerSize, QImageIOHandlerClipRect, QImageIOHandlerDescription, QImageIOHandlerScaledClipRect, QImageIOHandlerScaledSize, QImageIOHandlerCompressionRatio, QImageIOHandlerGamma, 
    QImageIOHandlerQuality, QImageIOHandlerName, QImageIOHandlerSubType, QImageIOHandlerIncrementalReading, QImageIOHandlerEndianness, QImageIOHandlerAnimation, QImageIOHandlerBackgroundColor );

procedure QImageIOHandler_setDevice(handle: QImageIOHandlerH; device: QIODeviceH); cdecl; external QtIntf name 'QImageIOHandler_setDevice';
function QImageIOHandler_device(handle: QImageIOHandlerH): QIODeviceH; cdecl; external QtIntf name 'QImageIOHandler_device';
procedure QImageIOHandler_setFormat(handle: QImageIOHandlerH; format: QByteArrayH); cdecl; external QtIntf name 'QImageIOHandler_setFormat';
procedure QImageIOHandler_format(handle: QImageIOHandlerH; retval: QByteArrayH); cdecl; external QtIntf name 'QImageIOHandler_format';
procedure QImageIOHandler_name(handle: QImageIOHandlerH; retval: QByteArrayH); cdecl; external QtIntf name 'QImageIOHandler_name';
function QImageIOHandler_canRead(handle: QImageIOHandlerH): Boolean; cdecl; external QtIntf name 'QImageIOHandler_canRead';
function QImageIOHandler_read(handle: QImageIOHandlerH; image: QImageH): Boolean; cdecl; external QtIntf name 'QImageIOHandler_read';
function QImageIOHandler_write(handle: QImageIOHandlerH; image: QImageH): Boolean; cdecl; external QtIntf name 'QImageIOHandler_write';
procedure QImageIOHandler_option(handle: QImageIOHandlerH; retval: QVariantH; option: QImageIOHandlerImageOption); cdecl; external QtIntf name 'QImageIOHandler_option';
procedure QImageIOHandler_setOption(handle: QImageIOHandlerH; option: QImageIOHandlerImageOption; value: QVariantH); cdecl; external QtIntf name 'QImageIOHandler_setOption';
function QImageIOHandler_supportsOption(handle: QImageIOHandlerH; option: QImageIOHandlerImageOption): Boolean; cdecl; external QtIntf name 'QImageIOHandler_supportsOption';
function QImageIOHandler_jumpToNextImage(handle: QImageIOHandlerH): Boolean; cdecl; external QtIntf name 'QImageIOHandler_jumpToNextImage';
function QImageIOHandler_jumpToImage(handle: QImageIOHandlerH; imageNumber: Integer): Boolean; cdecl; external QtIntf name 'QImageIOHandler_jumpToImage';
function QImageIOHandler_loopCount(handle: QImageIOHandlerH): Integer; cdecl; external QtIntf name 'QImageIOHandler_loopCount';
function QImageIOHandler_imageCount(handle: QImageIOHandlerH): Integer; cdecl; external QtIntf name 'QImageIOHandler_imageCount';
function QImageIOHandler_nextImageDelay(handle: QImageIOHandlerH): Integer; cdecl; external QtIntf name 'QImageIOHandler_nextImageDelay';
function QImageIOHandler_currentImageNumber(handle: QImageIOHandlerH): Integer; cdecl; external QtIntf name 'QImageIOHandler_currentImageNumber';
procedure QImageIOHandler_currentImageRect(handle: QImageIOHandlerH; retval: PRect); cdecl; external QtIntf name 'QImageIOHandler_currentImageRect';



type
  QImageReaderImageReaderError = ( // QImageReader::ImageReaderError (1)
    QImageReaderUnknownError, QImageReaderFileNotFoundError, QImageReaderDeviceError, QImageReaderUnsupportedFormatError, QImageReaderInvalidDataError );

function QImageReader_create(): QImageReaderH; overload; cdecl; external QtIntf name 'QImageReader_create';
procedure QImageReader_destroy(handle: QImageReaderH); cdecl; external QtIntf name 'QImageReader_destroy'; 
function QImageReader_create(device: QIODeviceH; format: QByteArrayH = nil): QImageReaderH; overload; cdecl; external QtIntf name 'QImageReader_create2';
function QImageReader_create(fileName: PWideString; format: QByteArrayH = nil): QImageReaderH; overload; cdecl; external QtIntf name 'QImageReader_create3';
procedure QImageReader_setFormat(handle: QImageReaderH; format: QByteArrayH); cdecl; external QtIntf name 'QImageReader_setFormat';
procedure QImageReader_format(handle: QImageReaderH; retval: QByteArrayH); cdecl; external QtIntf name 'QImageReader_format';
procedure QImageReader_setDevice(handle: QImageReaderH; device: QIODeviceH); cdecl; external QtIntf name 'QImageReader_setDevice';
function QImageReader_device(handle: QImageReaderH): QIODeviceH; cdecl; external QtIntf name 'QImageReader_device';
procedure QImageReader_setFileName(handle: QImageReaderH; fileName: PWideString); cdecl; external QtIntf name 'QImageReader_setFileName';
procedure QImageReader_fileName(handle: QImageReaderH; retval: PWideString); cdecl; external QtIntf name 'QImageReader_fileName';
procedure QImageReader_size(handle: QImageReaderH; retval: PSize); cdecl; external QtIntf name 'QImageReader_size';
procedure QImageReader_textKeys(handle: QImageReaderH; retval: QStringListH); cdecl; external QtIntf name 'QImageReader_textKeys';
procedure QImageReader_text(handle: QImageReaderH; retval: PWideString; key: PWideString); cdecl; external QtIntf name 'QImageReader_text';
procedure QImageReader_setClipRect(handle: QImageReaderH; rect: PRect); cdecl; external QtIntf name 'QImageReader_setClipRect';
procedure QImageReader_clipRect(handle: QImageReaderH; retval: PRect); cdecl; external QtIntf name 'QImageReader_clipRect';
procedure QImageReader_setScaledSize(handle: QImageReaderH; size: PSize); cdecl; external QtIntf name 'QImageReader_setScaledSize';
procedure QImageReader_scaledSize(handle: QImageReaderH; retval: PSize); cdecl; external QtIntf name 'QImageReader_scaledSize';
procedure QImageReader_setQuality(handle: QImageReaderH; quality: Integer); cdecl; external QtIntf name 'QImageReader_setQuality';
function QImageReader_quality(handle: QImageReaderH): Integer; cdecl; external QtIntf name 'QImageReader_quality';
procedure QImageReader_setScaledClipRect(handle: QImageReaderH; rect: PRect); cdecl; external QtIntf name 'QImageReader_setScaledClipRect';
procedure QImageReader_scaledClipRect(handle: QImageReaderH; retval: PRect); cdecl; external QtIntf name 'QImageReader_scaledClipRect';
procedure QImageReader_setBackgroundColor(handle: QImageReaderH; color: PQColor); cdecl; external QtIntf name 'QImageReader_setBackgroundColor';
procedure QImageReader_backgroundColor(handle: QImageReaderH; retval: PQColor); cdecl; external QtIntf name 'QImageReader_backgroundColor';
function QImageReader_supportsAnimation(handle: QImageReaderH): Boolean; cdecl; external QtIntf name 'QImageReader_supportsAnimation';
function QImageReader_canRead(handle: QImageReaderH): Boolean; cdecl; external QtIntf name 'QImageReader_canRead';
procedure QImageReader_read(handle: QImageReaderH; retval: QImageH); overload; cdecl; external QtIntf name 'QImageReader_read';
function QImageReader_jumpToNextImage(handle: QImageReaderH): Boolean; cdecl; external QtIntf name 'QImageReader_jumpToNextImage';
function QImageReader_jumpToImage(handle: QImageReaderH; imageNumber: Integer): Boolean; cdecl; external QtIntf name 'QImageReader_jumpToImage';
function QImageReader_loopCount(handle: QImageReaderH): Integer; cdecl; external QtIntf name 'QImageReader_loopCount';
function QImageReader_imageCount(handle: QImageReaderH): Integer; cdecl; external QtIntf name 'QImageReader_imageCount';
function QImageReader_nextImageDelay(handle: QImageReaderH): Integer; cdecl; external QtIntf name 'QImageReader_nextImageDelay';
function QImageReader_currentImageNumber(handle: QImageReaderH): Integer; cdecl; external QtIntf name 'QImageReader_currentImageNumber';
procedure QImageReader_currentImageRect(handle: QImageReaderH; retval: PRect); cdecl; external QtIntf name 'QImageReader_currentImageRect';
function QImageReader_error(handle: QImageReaderH): QImageReaderImageReaderError; cdecl; external QtIntf name 'QImageReader_error';
procedure QImageReader_errorString(handle: QImageReaderH; retval: PWideString); cdecl; external QtIntf name 'QImageReader_errorString';
function QImageReader_supportsOption(handle: QImageReaderH; option: QImageIOHandlerImageOption): Boolean; cdecl; external QtIntf name 'QImageReader_supportsOption';
procedure QImageReader_imageFormat(retval: QByteArrayH; fileName: PWideString); overload; cdecl; external QtIntf name 'QImageReader_imageFormat';
procedure QImageReader_imageFormat(retval: QByteArrayH; device: QIODeviceH); overload; cdecl; external QtIntf name 'QImageReader_imageFormat2';


type
  QImageWriterImageWriterError = ( // QImageWriter::ImageWriterError (1)
    QImageWriterUnknownError, QImageWriterDeviceError, QImageWriterUnsupportedFormatError );

function QImageWriter_create(): QImageWriterH; overload; cdecl; external QtIntf name 'QImageWriter_create';
procedure QImageWriter_destroy(handle: QImageWriterH); cdecl; external QtIntf name 'QImageWriter_destroy'; 
function QImageWriter_create(device: QIODeviceH; format: QByteArrayH): QImageWriterH; overload; cdecl; external QtIntf name 'QImageWriter_create2';
function QImageWriter_create(fileName: PWideString; format: QByteArrayH = nil): QImageWriterH; overload; cdecl; external QtIntf name 'QImageWriter_create3';
procedure QImageWriter_setFormat(handle: QImageWriterH; format: QByteArrayH); cdecl; external QtIntf name 'QImageWriter_setFormat';
procedure QImageWriter_format(handle: QImageWriterH; retval: QByteArrayH); cdecl; external QtIntf name 'QImageWriter_format';
procedure QImageWriter_setDevice(handle: QImageWriterH; device: QIODeviceH); cdecl; external QtIntf name 'QImageWriter_setDevice';
function QImageWriter_device(handle: QImageWriterH): QIODeviceH; cdecl; external QtIntf name 'QImageWriter_device';
procedure QImageWriter_setFileName(handle: QImageWriterH; fileName: PWideString); cdecl; external QtIntf name 'QImageWriter_setFileName';
procedure QImageWriter_fileName(handle: QImageWriterH; retval: PWideString); cdecl; external QtIntf name 'QImageWriter_fileName';
procedure QImageWriter_setQuality(handle: QImageWriterH; quality: Integer); cdecl; external QtIntf name 'QImageWriter_setQuality';
function QImageWriter_quality(handle: QImageWriterH): Integer; cdecl; external QtIntf name 'QImageWriter_quality';
procedure QImageWriter_setGamma(handle: QImageWriterH; gamma: Single); cdecl; external QtIntf name 'QImageWriter_setGamma';
function QImageWriter_gamma(handle: QImageWriterH): Single; cdecl; external QtIntf name 'QImageWriter_gamma';
procedure QImageWriter_setDescription(handle: QImageWriterH; description: PWideString); cdecl; external QtIntf name 'QImageWriter_setDescription';
procedure QImageWriter_description(handle: QImageWriterH; retval: PWideString); cdecl; external QtIntf name 'QImageWriter_description';
procedure QImageWriter_setText(handle: QImageWriterH; key: PWideString; text: PWideString); cdecl; external QtIntf name 'QImageWriter_setText';
function QImageWriter_canWrite(handle: QImageWriterH): Boolean; cdecl; external QtIntf name 'QImageWriter_canWrite';
function QImageWriter_write(handle: QImageWriterH; image: QImageH): Boolean; cdecl; external QtIntf name 'QImageWriter_write';
function QImageWriter_error(handle: QImageWriterH): QImageWriterImageWriterError; cdecl; external QtIntf name 'QImageWriter_error';
procedure QImageWriter_errorString(handle: QImageWriterH; retval: PWideString); cdecl; external QtIntf name 'QImageWriter_errorString';
function QImageWriter_supportsOption(handle: QImageWriterH; option: QImageIOHandlerImageOption): Boolean; cdecl; external QtIntf name 'QImageWriter_supportsOption';


type
  QValidatorState = ( // QValidator::State (1)
    QValidatorInvalid, QValidatorIntermediate, QValidatorAcceptable );

function QValidator_validate(handle: QValidatorH; p1: PWideString; p2: PInteger): QValidatorState; cdecl; external QtIntf name 'QValidator_validate';
procedure QValidator_fixup(handle: QValidatorH; p1: PWideString); cdecl; external QtIntf name 'QValidator_fixup';

function QIntValidator_create(parent: QObjectH): QIntValidatorH; overload; cdecl; external QtIntf name 'QIntValidator_create';
procedure QIntValidator_destroy(handle: QIntValidatorH); cdecl; external QtIntf name 'QIntValidator_destroy'; 
function QIntValidator_create(bottom: Integer; top: Integer; parent: QObjectH): QIntValidatorH; overload; cdecl; external QtIntf name 'QIntValidator_create2';
function QIntValidator_validate(handle: QIntValidatorH; p1: PWideString; p2: PInteger): QValidatorState; cdecl; external QtIntf name 'QIntValidator_validate';
procedure QIntValidator_setBottom(handle: QIntValidatorH; p1: Integer); cdecl; external QtIntf name 'QIntValidator_setBottom';
procedure QIntValidator_setTop(handle: QIntValidatorH; p1: Integer); cdecl; external QtIntf name 'QIntValidator_setTop';
procedure QIntValidator_setRange(handle: QIntValidatorH; bottom: Integer; top: Integer); cdecl; external QtIntf name 'QIntValidator_setRange';
function QIntValidator_bottom(handle: QIntValidatorH): Integer; cdecl; external QtIntf name 'QIntValidator_bottom';
function QIntValidator_top(handle: QIntValidatorH): Integer; cdecl; external QtIntf name 'QIntValidator_top';

function QDoubleValidator_create(parent: QObjectH): QDoubleValidatorH; overload; cdecl; external QtIntf name 'QDoubleValidator_create';
procedure QDoubleValidator_destroy(handle: QDoubleValidatorH); cdecl; external QtIntf name 'QDoubleValidator_destroy'; 
function QDoubleValidator_create(bottom: Double; top: Double; decimals: Integer; parent: QObjectH): QDoubleValidatorH; overload; cdecl; external QtIntf name 'QDoubleValidator_create2';
function QDoubleValidator_validate(handle: QDoubleValidatorH; p1: PWideString; p2: PInteger): QValidatorState; cdecl; external QtIntf name 'QDoubleValidator_validate';
procedure QDoubleValidator_setRange(handle: QDoubleValidatorH; bottom: Double; top: Double; decimals: Integer = 0); cdecl; external QtIntf name 'QDoubleValidator_setRange';
procedure QDoubleValidator_setBottom(handle: QDoubleValidatorH; p1: Double); cdecl; external QtIntf name 'QDoubleValidator_setBottom';
procedure QDoubleValidator_setTop(handle: QDoubleValidatorH; p1: Double); cdecl; external QtIntf name 'QDoubleValidator_setTop';
procedure QDoubleValidator_setDecimals(handle: QDoubleValidatorH; p1: Integer); cdecl; external QtIntf name 'QDoubleValidator_setDecimals';
function QDoubleValidator_bottom(handle: QDoubleValidatorH): Double; cdecl; external QtIntf name 'QDoubleValidator_bottom';
function QDoubleValidator_top(handle: QDoubleValidatorH): Double; cdecl; external QtIntf name 'QDoubleValidator_top';
function QDoubleValidator_decimals(handle: QDoubleValidatorH): Integer; cdecl; external QtIntf name 'QDoubleValidator_decimals';

function QRegExpValidator_create(parent: QObjectH): QRegExpValidatorH; overload; cdecl; external QtIntf name 'QRegExpValidator_create';
procedure QRegExpValidator_destroy(handle: QRegExpValidatorH); cdecl; external QtIntf name 'QRegExpValidator_destroy'; 
function QRegExpValidator_create(rx: QRegExpH; parent: QObjectH): QRegExpValidatorH; overload; cdecl; external QtIntf name 'QRegExpValidator_create2';
function QRegExpValidator_validate(handle: QRegExpValidatorH; input: PWideString; pos: PInteger): QValidatorState; cdecl; external QtIntf name 'QRegExpValidator_validate';
procedure QRegExpValidator_setRegExp(handle: QRegExpValidatorH; rx: QRegExpH); cdecl; external QtIntf name 'QRegExpValidator_setRegExp';
function QRegExpValidator_regExp(handle: QRegExpValidatorH): QRegExpH; cdecl; external QtIntf name 'QRegExpValidator_regExp';


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

  QFrameStyleMask = (  //QFrame::StyleMask (2s)
    QFrameShape_Mask = $000f,
    QFrameShadow_Mask = $00f0 );

function QFrame_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QFrameH; cdecl; external QtIntf name 'QFrame_create';
procedure QFrame_destroy(handle: QFrameH); cdecl; external QtIntf name 'QFrame_destroy'; 
function QFrame_frameStyle(handle: QFrameH): Integer; cdecl; external QtIntf name 'QFrame_frameStyle';
procedure QFrame_setFrameStyle(handle: QFrameH; p1: Integer); cdecl; external QtIntf name 'QFrame_setFrameStyle';
function QFrame_frameWidth(handle: QFrameH): Integer; cdecl; external QtIntf name 'QFrame_frameWidth';
procedure QFrame_sizeHint(handle: QFrameH; retval: PSize); cdecl; external QtIntf name 'QFrame_sizeHint';
function QFrame_frameShape(handle: QFrameH): QFrameShape; cdecl; external QtIntf name 'QFrame_frameShape';
procedure QFrame_setFrameShape(handle: QFrameH; p1: QFrameShape); cdecl; external QtIntf name 'QFrame_setFrameShape';
function QFrame_frameShadow(handle: QFrameH): QFrameShadow; cdecl; external QtIntf name 'QFrame_frameShadow';
procedure QFrame_setFrameShadow(handle: QFrameH; p1: QFrameShadow); cdecl; external QtIntf name 'QFrame_setFrameShadow';
function QFrame_lineWidth(handle: QFrameH): Integer; cdecl; external QtIntf name 'QFrame_lineWidth';
procedure QFrame_setLineWidth(handle: QFrameH; p1: Integer); cdecl; external QtIntf name 'QFrame_setLineWidth';
function QFrame_midLineWidth(handle: QFrameH): Integer; cdecl; external QtIntf name 'QFrame_midLineWidth';
procedure QFrame_setMidLineWidth(handle: QFrameH; p1: Integer); cdecl; external QtIntf name 'QFrame_setMidLineWidth';
procedure QFrame_frameRect(handle: QFrameH; retval: PRect); cdecl; external QtIntf name 'QFrame_frameRect';
procedure QFrame_setFrameRect(handle: QFrameH; p1: PRect); cdecl; external QtIntf name 'QFrame_setFrameRect';

function QAbstractScrollArea_create(parent: QWidgetH = nil): QAbstractScrollAreaH; cdecl; external QtIntf name 'QAbstractScrollArea_create';
procedure QAbstractScrollArea_destroy(handle: QAbstractScrollAreaH); cdecl; external QtIntf name 'QAbstractScrollArea_destroy'; 
function QAbstractScrollArea_verticalScrollBarPolicy(handle: QAbstractScrollAreaH): QtScrollBarPolicy; cdecl; external QtIntf name 'QAbstractScrollArea_verticalScrollBarPolicy';
procedure QAbstractScrollArea_setVerticalScrollBarPolicy(handle: QAbstractScrollAreaH; p1: QtScrollBarPolicy); cdecl; external QtIntf name 'QAbstractScrollArea_setVerticalScrollBarPolicy';
function QAbstractScrollArea_verticalScrollBar(handle: QAbstractScrollAreaH): QScrollBarH; cdecl; external QtIntf name 'QAbstractScrollArea_verticalScrollBar';
procedure QAbstractScrollArea_setVerticalScrollBar(handle: QAbstractScrollAreaH; scrollbar: QScrollBarH); cdecl; external QtIntf name 'QAbstractScrollArea_setVerticalScrollBar';
function QAbstractScrollArea_horizontalScrollBarPolicy(handle: QAbstractScrollAreaH): QtScrollBarPolicy; cdecl; external QtIntf name 'QAbstractScrollArea_horizontalScrollBarPolicy';
procedure QAbstractScrollArea_setHorizontalScrollBarPolicy(handle: QAbstractScrollAreaH; p1: QtScrollBarPolicy); cdecl; external QtIntf name 'QAbstractScrollArea_setHorizontalScrollBarPolicy';
function QAbstractScrollArea_horizontalScrollBar(handle: QAbstractScrollAreaH): QScrollBarH; cdecl; external QtIntf name 'QAbstractScrollArea_horizontalScrollBar';
procedure QAbstractScrollArea_setHorizontalScrollBar(handle: QAbstractScrollAreaH; scrollbar: QScrollBarH); cdecl; external QtIntf name 'QAbstractScrollArea_setHorizontalScrollBar';
function QAbstractScrollArea_cornerWidget(handle: QAbstractScrollAreaH): QWidgetH; cdecl; external QtIntf name 'QAbstractScrollArea_cornerWidget';
procedure QAbstractScrollArea_setCornerWidget(handle: QAbstractScrollAreaH; widget: QWidgetH); cdecl; external QtIntf name 'QAbstractScrollArea_setCornerWidget';
procedure QAbstractScrollArea_addScrollBarWidget(handle: QAbstractScrollAreaH; widget: QWidgetH; alignment: QtAlignment); cdecl; external QtIntf name 'QAbstractScrollArea_addScrollBarWidget';
procedure QAbstractScrollArea_scrollBarWidgets(handle: QAbstractScrollAreaH; retval: PIntArray; alignment: QtAlignment); cdecl; external QtIntf name 'QAbstractScrollArea_scrollBarWidgets';
function QAbstractScrollArea_viewport(handle: QAbstractScrollAreaH): QWidgetH; cdecl; external QtIntf name 'QAbstractScrollArea_viewport';
procedure QAbstractScrollArea_setViewport(handle: QAbstractScrollAreaH; widget: QWidgetH); cdecl; external QtIntf name 'QAbstractScrollArea_setViewport';
procedure QAbstractScrollArea_maximumViewportSize(handle: QAbstractScrollAreaH; retval: PSize); cdecl; external QtIntf name 'QAbstractScrollArea_maximumViewportSize';
procedure QAbstractScrollArea_minimumSizeHint(handle: QAbstractScrollAreaH; retval: PSize); cdecl; external QtIntf name 'QAbstractScrollArea_minimumSizeHint';
procedure QAbstractScrollArea_sizeHint(handle: QAbstractScrollAreaH; retval: PSize); cdecl; external QtIntf name 'QAbstractScrollArea_sizeHint';

function QLCLAbstractScrollArea_create(parent: QWidgetH = nil): QLCLAbstractScrollAreaH; cdecl; external QtIntf name 'QLCLAbstractScrollArea_create';
procedure QLCLAbstractScrollArea_destroy(handle: QLCLAbstractScrollAreaH); cdecl; external QtIntf name 'QLCLAbstractScrollArea_destroy'; 
procedure QLCLAbstractScrollArea_override_viewportEvent(handle: QLCLAbstractScrollAreaH; hook: QHookH); cdecl; external QtIntf name 'QLCLAbstractScrollArea_override_viewportEvent';
function QLCLAbstractScrollArea_InheritedViewportEvent(handle: QLCLAbstractScrollAreaH; event: QEventH): Boolean; cdecl; external QtIntf name 'QLCLAbstractScrollArea_InheritedViewportEvent';


type
  QAbstractSliderSliderAction = ( // QAbstractSlider::SliderAction (1)
    QAbstractSliderSliderNoAction, QAbstractSliderSliderSingleStepAdd, QAbstractSliderSliderSingleStepSub, QAbstractSliderSliderPageStepAdd, QAbstractSliderSliderPageStepSub, QAbstractSliderSliderToMinimum, 
    QAbstractSliderSliderToMaximum, QAbstractSliderSliderMove );

function QAbstractSlider_create(parent: QWidgetH = nil): QAbstractSliderH; cdecl; external QtIntf name 'QAbstractSlider_create';
procedure QAbstractSlider_destroy(handle: QAbstractSliderH); cdecl; external QtIntf name 'QAbstractSlider_destroy'; 
function QAbstractSlider_orientation(handle: QAbstractSliderH): QtOrientation; cdecl; external QtIntf name 'QAbstractSlider_orientation';
procedure QAbstractSlider_setMinimum(handle: QAbstractSliderH; p1: Integer); cdecl; external QtIntf name 'QAbstractSlider_setMinimum';
function QAbstractSlider_minimum(handle: QAbstractSliderH): Integer; cdecl; external QtIntf name 'QAbstractSlider_minimum';
procedure QAbstractSlider_setMaximum(handle: QAbstractSliderH; p1: Integer); cdecl; external QtIntf name 'QAbstractSlider_setMaximum';
function QAbstractSlider_maximum(handle: QAbstractSliderH): Integer; cdecl; external QtIntf name 'QAbstractSlider_maximum';
procedure QAbstractSlider_setRange(handle: QAbstractSliderH; min: Integer; max: Integer); cdecl; external QtIntf name 'QAbstractSlider_setRange';
procedure QAbstractSlider_setSingleStep(handle: QAbstractSliderH; p1: Integer); cdecl; external QtIntf name 'QAbstractSlider_setSingleStep';
function QAbstractSlider_singleStep(handle: QAbstractSliderH): Integer; cdecl; external QtIntf name 'QAbstractSlider_singleStep';
procedure QAbstractSlider_setPageStep(handle: QAbstractSliderH; p1: Integer); cdecl; external QtIntf name 'QAbstractSlider_setPageStep';
function QAbstractSlider_pageStep(handle: QAbstractSliderH): Integer; cdecl; external QtIntf name 'QAbstractSlider_pageStep';
procedure QAbstractSlider_setTracking(handle: QAbstractSliderH; enable: Boolean); cdecl; external QtIntf name 'QAbstractSlider_setTracking';
function QAbstractSlider_hasTracking(handle: QAbstractSliderH): Boolean; cdecl; external QtIntf name 'QAbstractSlider_hasTracking';
procedure QAbstractSlider_setSliderDown(handle: QAbstractSliderH; p1: Boolean); cdecl; external QtIntf name 'QAbstractSlider_setSliderDown';
function QAbstractSlider_isSliderDown(handle: QAbstractSliderH): Boolean; cdecl; external QtIntf name 'QAbstractSlider_isSliderDown';
procedure QAbstractSlider_setSliderPosition(handle: QAbstractSliderH; p1: Integer); cdecl; external QtIntf name 'QAbstractSlider_setSliderPosition';
function QAbstractSlider_sliderPosition(handle: QAbstractSliderH): Integer; cdecl; external QtIntf name 'QAbstractSlider_sliderPosition';
procedure QAbstractSlider_setInvertedAppearance(handle: QAbstractSliderH; p1: Boolean); cdecl; external QtIntf name 'QAbstractSlider_setInvertedAppearance';
function QAbstractSlider_invertedAppearance(handle: QAbstractSliderH): Boolean; cdecl; external QtIntf name 'QAbstractSlider_invertedAppearance';
procedure QAbstractSlider_setInvertedControls(handle: QAbstractSliderH; p1: Boolean); cdecl; external QtIntf name 'QAbstractSlider_setInvertedControls';
function QAbstractSlider_invertedControls(handle: QAbstractSliderH): Boolean; cdecl; external QtIntf name 'QAbstractSlider_invertedControls';
function QAbstractSlider_value(handle: QAbstractSliderH): Integer; cdecl; external QtIntf name 'QAbstractSlider_value';
procedure QAbstractSlider_triggerAction(handle: QAbstractSliderH; action: QAbstractSliderSliderAction); cdecl; external QtIntf name 'QAbstractSlider_triggerAction';
procedure QAbstractSlider_setValue(handle: QAbstractSliderH; p1: Integer); cdecl; external QtIntf name 'QAbstractSlider_setValue';
procedure QAbstractSlider_setOrientation(handle: QAbstractSliderH; p1: QtOrientation); cdecl; external QtIntf name 'QAbstractSlider_setOrientation';


type
  QAbstractSlider_valueChanged_Event = procedure (value: Integer) of object cdecl;
  QAbstractSlider_sliderPressed_Event = procedure () of object cdecl;
  QAbstractSlider_sliderMoved_Event = procedure (position: Integer) of object cdecl;
  QAbstractSlider_sliderReleased_Event = procedure () of object cdecl;
  QAbstractSlider_rangeChanged_Event = procedure (min: Integer; max: Integer) of object cdecl;
  QAbstractSlider_actionTriggered_Event = procedure (action: Integer) of object cdecl;


function QScrollBar_create(parent: QWidgetH = nil): QScrollBarH; overload; cdecl; external QtIntf name 'QScrollBar_create';
procedure QScrollBar_destroy(handle: QScrollBarH); cdecl; external QtIntf name 'QScrollBar_destroy'; 
function QScrollBar_create(p1: QtOrientation; parent: QWidgetH = nil): QScrollBarH; overload; cdecl; external QtIntf name 'QScrollBar_create2';
procedure QScrollBar_sizeHint(handle: QScrollBarH; retval: PSize); cdecl; external QtIntf name 'QScrollBar_sizeHint';
function QScrollBar_event(handle: QScrollBarH; event: QEventH): Boolean; cdecl; external QtIntf name 'QScrollBar_event';

function QMenu_create(parent: QWidgetH = nil): QMenuH; overload; cdecl; external QtIntf name 'QMenu_create';
procedure QMenu_destroy(handle: QMenuH); cdecl; external QtIntf name 'QMenu_destroy'; 
function QMenu_create(title: PWideString; parent: QWidgetH = nil): QMenuH; overload; cdecl; external QtIntf name 'QMenu_create2';
function QMenu_addAction(handle: QMenuH; text: PWideString): QActionH; overload; cdecl; external QtIntf name 'QMenu_addAction';
function QMenu_addAction(handle: QMenuH; icon: QIconH; text: PWideString): QActionH; overload; cdecl; external QtIntf name 'QMenu_addAction2';
function QMenu_addAction(handle: QMenuH; text: PWideString; receiver: QObjectH; member: PAnsiChar; shortcut: QKeySequenceH = nil): QActionH; overload; cdecl; external QtIntf name 'QMenu_addAction3';
function QMenu_addAction(handle: QMenuH; icon: QIconH; text: PWideString; receiver: QObjectH; member: PAnsiChar; shortcut: QKeySequenceH = nil): QActionH; overload; cdecl; external QtIntf name 'QMenu_addAction4';
function QMenu_addMenu(handle: QMenuH; menu: QMenuH): QActionH; overload; cdecl; external QtIntf name 'QMenu_addMenu';
function QMenu_addMenu(handle: QMenuH; title: PWideString): QMenuH; overload; cdecl; external QtIntf name 'QMenu_addMenu2';
function QMenu_addMenu(handle: QMenuH; icon: QIconH; title: PWideString): QMenuH; overload; cdecl; external QtIntf name 'QMenu_addMenu3';
function QMenu_addSeparator(handle: QMenuH): QActionH; cdecl; external QtIntf name 'QMenu_addSeparator';
function QMenu_insertMenu(handle: QMenuH; before: QActionH; menu: QMenuH): QActionH; cdecl; external QtIntf name 'QMenu_insertMenu';
function QMenu_insertSeparator(handle: QMenuH; before: QActionH): QActionH; cdecl; external QtIntf name 'QMenu_insertSeparator';
function QMenu_isEmpty(handle: QMenuH): Boolean; cdecl; external QtIntf name 'QMenu_isEmpty';
procedure QMenu_clear(handle: QMenuH); cdecl; external QtIntf name 'QMenu_clear';
procedure QMenu_setTearOffEnabled(handle: QMenuH; p1: Boolean); cdecl; external QtIntf name 'QMenu_setTearOffEnabled';
function QMenu_isTearOffEnabled(handle: QMenuH): Boolean; cdecl; external QtIntf name 'QMenu_isTearOffEnabled';
function QMenu_isTearOffMenuVisible(handle: QMenuH): Boolean; cdecl; external QtIntf name 'QMenu_isTearOffMenuVisible';
procedure QMenu_hideTearOffMenu(handle: QMenuH); cdecl; external QtIntf name 'QMenu_hideTearOffMenu';
procedure QMenu_setDefaultAction(handle: QMenuH; p1: QActionH); cdecl; external QtIntf name 'QMenu_setDefaultAction';
function QMenu_defaultAction(handle: QMenuH): QActionH; cdecl; external QtIntf name 'QMenu_defaultAction';
procedure QMenu_setActiveAction(handle: QMenuH; act: QActionH); cdecl; external QtIntf name 'QMenu_setActiveAction';
function QMenu_activeAction(handle: QMenuH): QActionH; cdecl; external QtIntf name 'QMenu_activeAction';
procedure QMenu_popup(handle: QMenuH; pos: PQtPoint; at: QActionH = nil); cdecl; external QtIntf name 'QMenu_popup';
function QMenu_exec(handle: QMenuH): QActionH; overload; cdecl; external QtIntf name 'QMenu_exec';
function QMenu_exec(handle: QMenuH; pos: PQtPoint; at: QActionH = nil): QActionH; overload; cdecl; external QtIntf name 'QMenu_exec2';
function QMenu_exec(actions: PIntArray; pos: PQtPoint; at: QActionH = nil): QActionH; overload; cdecl; external QtIntf name 'QMenu_exec3';
procedure QMenu_sizeHint(handle: QMenuH; retval: PSize); cdecl; external QtIntf name 'QMenu_sizeHint';
procedure QMenu_actionGeometry(handle: QMenuH; retval: PRect; p1: QActionH); cdecl; external QtIntf name 'QMenu_actionGeometry';
function QMenu_actionAt(handle: QMenuH; p1: PQtPoint): QActionH; cdecl; external QtIntf name 'QMenu_actionAt';
function QMenu_menuAction(handle: QMenuH): QActionH; cdecl; external QtIntf name 'QMenu_menuAction';
procedure QMenu_title(handle: QMenuH; retval: PWideString); cdecl; external QtIntf name 'QMenu_title';
procedure QMenu_setTitle(handle: QMenuH; title: PWideString); cdecl; external QtIntf name 'QMenu_setTitle';
procedure QMenu_icon(handle: QMenuH; retval: QIconH); cdecl; external QtIntf name 'QMenu_icon';
procedure QMenu_setIcon(handle: QMenuH; icon: QIconH); cdecl; external QtIntf name 'QMenu_setIcon';
procedure QMenu_setNoReplayFor(handle: QMenuH; widget: QWidgetH); cdecl; external QtIntf name 'QMenu_setNoReplayFor';
function QMenu_separatorsCollapsible(handle: QMenuH): Boolean; cdecl; external QtIntf name 'QMenu_separatorsCollapsible';
procedure QMenu_setSeparatorsCollapsible(handle: QMenuH; collapse: Boolean); cdecl; external QtIntf name 'QMenu_setSeparatorsCollapsible';
{$ifdef DARWIN }
function QMenu_macMenu(handle: QMenuH; merge: MenuRef = 0): MenuRef; cdecl; external QtIntf name 'QMenu_macMenu';
{$endif}


type
  QMenu_aboutToShow_Event = procedure () of object cdecl;
  QMenu_aboutToHide_Event = procedure () of object cdecl;
  QMenu_triggered_Event = procedure (action: QActionH) of object cdecl;
  QMenu_hovered_Event = procedure (action: QActionH) of object cdecl;


function QMenuBar_create(parent: QWidgetH = nil): QMenuBarH; cdecl; external QtIntf name 'QMenuBar_create';
procedure QMenuBar_destroy(handle: QMenuBarH); cdecl; external QtIntf name 'QMenuBar_destroy'; 
function QMenuBar_addAction(handle: QMenuBarH; text: PWideString): QActionH; overload; cdecl; external QtIntf name 'QMenuBar_addAction';
function QMenuBar_addAction(handle: QMenuBarH; text: PWideString; receiver: QObjectH; member: PAnsiChar): QActionH; overload; cdecl; external QtIntf name 'QMenuBar_addAction2';
function QMenuBar_addMenu(handle: QMenuBarH; menu: QMenuH): QActionH; overload; cdecl; external QtIntf name 'QMenuBar_addMenu';
function QMenuBar_addMenu(handle: QMenuBarH; title: PWideString): QMenuH; overload; cdecl; external QtIntf name 'QMenuBar_addMenu2';
function QMenuBar_addMenu(handle: QMenuBarH; icon: QIconH; title: PWideString): QMenuH; overload; cdecl; external QtIntf name 'QMenuBar_addMenu3';
function QMenuBar_addSeparator(handle: QMenuBarH): QActionH; cdecl; external QtIntf name 'QMenuBar_addSeparator';
function QMenuBar_insertSeparator(handle: QMenuBarH; before: QActionH): QActionH; cdecl; external QtIntf name 'QMenuBar_insertSeparator';
function QMenuBar_insertMenu(handle: QMenuBarH; before: QActionH; menu: QMenuH): QActionH; cdecl; external QtIntf name 'QMenuBar_insertMenu';
procedure QMenuBar_clear(handle: QMenuBarH); cdecl; external QtIntf name 'QMenuBar_clear';
function QMenuBar_activeAction(handle: QMenuBarH): QActionH; cdecl; external QtIntf name 'QMenuBar_activeAction';
procedure QMenuBar_setActiveAction(handle: QMenuBarH; action: QActionH); cdecl; external QtIntf name 'QMenuBar_setActiveAction';
procedure QMenuBar_setDefaultUp(handle: QMenuBarH; p1: Boolean); cdecl; external QtIntf name 'QMenuBar_setDefaultUp';
function QMenuBar_isDefaultUp(handle: QMenuBarH): Boolean; cdecl; external QtIntf name 'QMenuBar_isDefaultUp';
procedure QMenuBar_sizeHint(handle: QMenuBarH; retval: PSize); cdecl; external QtIntf name 'QMenuBar_sizeHint';
procedure QMenuBar_minimumSizeHint(handle: QMenuBarH; retval: PSize); cdecl; external QtIntf name 'QMenuBar_minimumSizeHint';
function QMenuBar_heightForWidth(handle: QMenuBarH; p1: Integer): Integer; cdecl; external QtIntf name 'QMenuBar_heightForWidth';
procedure QMenuBar_actionGeometry(handle: QMenuBarH; retval: PRect; p1: QActionH); cdecl; external QtIntf name 'QMenuBar_actionGeometry';
function QMenuBar_actionAt(handle: QMenuBarH; p1: PQtPoint): QActionH; cdecl; external QtIntf name 'QMenuBar_actionAt';
procedure QMenuBar_setCornerWidget(handle: QMenuBarH; w: QWidgetH; corner: QtCorner = QtTopRightCorner); cdecl; external QtIntf name 'QMenuBar_setCornerWidget';
function QMenuBar_cornerWidget(handle: QMenuBarH; corner: QtCorner = QtTopRightCorner): QWidgetH; cdecl; external QtIntf name 'QMenuBar_cornerWidget';
{$ifdef DARWIN }
function QMenuBar_macMenu(handle: QMenuBarH): MenuRef; cdecl; external QtIntf name 'QMenuBar_macMenu';
{$endif}


type
  QMenuBar_triggered_Event = procedure (action: QActionH) of object cdecl;
  QMenuBar_hovered_Event = procedure (action: QActionH) of object cdecl;


function QButtonGroup_create(parent: QObjectH = nil): QButtonGroupH; cdecl; external QtIntf name 'QButtonGroup_create';
procedure QButtonGroup_destroy(handle: QButtonGroupH); cdecl; external QtIntf name 'QButtonGroup_destroy'; 
procedure QButtonGroup_setExclusive(handle: QButtonGroupH; p1: Boolean); cdecl; external QtIntf name 'QButtonGroup_setExclusive';
function QButtonGroup_exclusive(handle: QButtonGroupH): Boolean; cdecl; external QtIntf name 'QButtonGroup_exclusive';
procedure QButtonGroup_addButton(handle: QButtonGroupH; p1: QAbstractButtonH); overload; cdecl; external QtIntf name 'QButtonGroup_addButton';
procedure QButtonGroup_addButton(handle: QButtonGroupH; p1: QAbstractButtonH; id: Integer); overload; cdecl; external QtIntf name 'QButtonGroup_addButton2';
procedure QButtonGroup_removeButton(handle: QButtonGroupH; p1: QAbstractButtonH); cdecl; external QtIntf name 'QButtonGroup_removeButton';
procedure QButtonGroup_buttons(handle: QButtonGroupH; retval: PIntArray); cdecl; external QtIntf name 'QButtonGroup_buttons';
function QButtonGroup_checkedButton(handle: QButtonGroupH): QAbstractButtonH; cdecl; external QtIntf name 'QButtonGroup_checkedButton';
function QButtonGroup_button(handle: QButtonGroupH; id: Integer): QAbstractButtonH; cdecl; external QtIntf name 'QButtonGroup_button';
procedure QButtonGroup_setId(handle: QButtonGroupH; button: QAbstractButtonH; id: Integer); cdecl; external QtIntf name 'QButtonGroup_setId';
function QButtonGroup_id(handle: QButtonGroupH; button: QAbstractButtonH): Integer; cdecl; external QtIntf name 'QButtonGroup_id';
function QButtonGroup_checkedId(handle: QButtonGroupH): Integer; cdecl; external QtIntf name 'QButtonGroup_checkedId';


type
  QButtonGroup_buttonClicked_Event = procedure (p1: QAbstractButtonH) of object cdecl;
  QButtonGroup_buttonClicked2_Event = procedure (p1: Integer) of object cdecl;
  QButtonGroup_buttonPressed_Event = procedure (p1: QAbstractButtonH) of object cdecl;
  QButtonGroup_buttonPressed2_Event = procedure (p1: Integer) of object cdecl;
  QButtonGroup_buttonReleased_Event = procedure (p1: QAbstractButtonH) of object cdecl;
  QButtonGroup_buttonReleased2_Event = procedure (p1: Integer) of object cdecl;


procedure QAbstractButton_setText(handle: QAbstractButtonH; text: PWideString); cdecl; external QtIntf name 'QAbstractButton_setText';
procedure QAbstractButton_text(handle: QAbstractButtonH; retval: PWideString); cdecl; external QtIntf name 'QAbstractButton_text';
procedure QAbstractButton_setIcon(handle: QAbstractButtonH; icon: QIconH); cdecl; external QtIntf name 'QAbstractButton_setIcon';
procedure QAbstractButton_icon(handle: QAbstractButtonH; retval: QIconH); cdecl; external QtIntf name 'QAbstractButton_icon';
procedure QAbstractButton_iconSize(handle: QAbstractButtonH; retval: PSize); cdecl; external QtIntf name 'QAbstractButton_iconSize';
procedure QAbstractButton_setShortcut(handle: QAbstractButtonH; key: QKeySequenceH); cdecl; external QtIntf name 'QAbstractButton_setShortcut';
procedure QAbstractButton_shortcut(handle: QAbstractButtonH; retval: QKeySequenceH); cdecl; external QtIntf name 'QAbstractButton_shortcut';
procedure QAbstractButton_setCheckable(handle: QAbstractButtonH; p1: Boolean); cdecl; external QtIntf name 'QAbstractButton_setCheckable';
function QAbstractButton_isCheckable(handle: QAbstractButtonH): Boolean; cdecl; external QtIntf name 'QAbstractButton_isCheckable';
function QAbstractButton_isChecked(handle: QAbstractButtonH): Boolean; cdecl; external QtIntf name 'QAbstractButton_isChecked';
procedure QAbstractButton_setDown(handle: QAbstractButtonH; p1: Boolean); cdecl; external QtIntf name 'QAbstractButton_setDown';
function QAbstractButton_isDown(handle: QAbstractButtonH): Boolean; cdecl; external QtIntf name 'QAbstractButton_isDown';
procedure QAbstractButton_setAutoRepeat(handle: QAbstractButtonH; p1: Boolean); cdecl; external QtIntf name 'QAbstractButton_setAutoRepeat';
function QAbstractButton_autoRepeat(handle: QAbstractButtonH): Boolean; cdecl; external QtIntf name 'QAbstractButton_autoRepeat';
procedure QAbstractButton_setAutoRepeatDelay(handle: QAbstractButtonH; p1: Integer); cdecl; external QtIntf name 'QAbstractButton_setAutoRepeatDelay';
function QAbstractButton_autoRepeatDelay(handle: QAbstractButtonH): Integer; cdecl; external QtIntf name 'QAbstractButton_autoRepeatDelay';
procedure QAbstractButton_setAutoRepeatInterval(handle: QAbstractButtonH; p1: Integer); cdecl; external QtIntf name 'QAbstractButton_setAutoRepeatInterval';
function QAbstractButton_autoRepeatInterval(handle: QAbstractButtonH): Integer; cdecl; external QtIntf name 'QAbstractButton_autoRepeatInterval';
procedure QAbstractButton_setAutoExclusive(handle: QAbstractButtonH; p1: Boolean); cdecl; external QtIntf name 'QAbstractButton_setAutoExclusive';
function QAbstractButton_autoExclusive(handle: QAbstractButtonH): Boolean; cdecl; external QtIntf name 'QAbstractButton_autoExclusive';
function QAbstractButton_group(handle: QAbstractButtonH): QButtonGroupH; cdecl; external QtIntf name 'QAbstractButton_group';
procedure QAbstractButton_setIconSize(handle: QAbstractButtonH; size: PSize); cdecl; external QtIntf name 'QAbstractButton_setIconSize';
procedure QAbstractButton_animateClick(handle: QAbstractButtonH; msec: Integer = 100); cdecl; external QtIntf name 'QAbstractButton_animateClick';
procedure QAbstractButton_click(handle: QAbstractButtonH); cdecl; external QtIntf name 'QAbstractButton_click';
procedure QAbstractButton_toggle(handle: QAbstractButtonH); cdecl; external QtIntf name 'QAbstractButton_toggle';
procedure QAbstractButton_setChecked(handle: QAbstractButtonH; p1: Boolean); cdecl; external QtIntf name 'QAbstractButton_setChecked';


type
  QAbstractButton_pressed_Event = procedure () of object cdecl;
  QAbstractButton_released_Event = procedure () of object cdecl;
  QAbstractButton_clicked_Event = procedure (checked: Boolean = False) of object cdecl;
  QAbstractButton_clicked2_Event = procedure () of object cdecl;
  QAbstractButton_toggled_Event = procedure (checked: Boolean) of object cdecl;


function QPushButton_create(parent: QWidgetH = nil): QPushButtonH; overload; cdecl; external QtIntf name 'QPushButton_create';
procedure QPushButton_destroy(handle: QPushButtonH); cdecl; external QtIntf name 'QPushButton_destroy'; 
function QPushButton_create(text: PWideString; parent: QWidgetH = nil): QPushButtonH; overload; cdecl; external QtIntf name 'QPushButton_create2';
function QPushButton_create(icon: QIconH; text: PWideString; parent: QWidgetH = nil): QPushButtonH; overload; cdecl; external QtIntf name 'QPushButton_create3';
procedure QPushButton_sizeHint(handle: QPushButtonH; retval: PSize); cdecl; external QtIntf name 'QPushButton_sizeHint';
function QPushButton_autoDefault(handle: QPushButtonH): Boolean; cdecl; external QtIntf name 'QPushButton_autoDefault';
procedure QPushButton_setAutoDefault(handle: QPushButtonH; p1: Boolean); cdecl; external QtIntf name 'QPushButton_setAutoDefault';
function QPushButton_isDefault(handle: QPushButtonH): Boolean; cdecl; external QtIntf name 'QPushButton_isDefault';
procedure QPushButton_setDefault(handle: QPushButtonH; p1: Boolean); cdecl; external QtIntf name 'QPushButton_setDefault';
procedure QPushButton_setMenu(handle: QPushButtonH; menu: QMenuH); cdecl; external QtIntf name 'QPushButton_setMenu';
function QPushButton_menu(handle: QPushButtonH): QMenuH; cdecl; external QtIntf name 'QPushButton_menu';
procedure QPushButton_setFlat(handle: QPushButtonH; p1: Boolean); cdecl; external QtIntf name 'QPushButton_setFlat';
function QPushButton_isFlat(handle: QPushButtonH): Boolean; cdecl; external QtIntf name 'QPushButton_isFlat';
procedure QPushButton_showMenu(handle: QPushButtonH); cdecl; external QtIntf name 'QPushButton_showMenu';

function QRadioButton_create(parent: QWidgetH = nil): QRadioButtonH; overload; cdecl; external QtIntf name 'QRadioButton_create';
procedure QRadioButton_destroy(handle: QRadioButtonH); cdecl; external QtIntf name 'QRadioButton_destroy'; 
function QRadioButton_create(text: PWideString; parent: QWidgetH = nil): QRadioButtonH; overload; cdecl; external QtIntf name 'QRadioButton_create2';
procedure QRadioButton_sizeHint(handle: QRadioButtonH; retval: PSize); cdecl; external QtIntf name 'QRadioButton_sizeHint';


type
  QLineEditEchoMode = ( // QLineEdit::EchoMode (1)
    QLineEditNormal, QLineEditNoEcho, QLineEditPassword, QLineEditPasswordEchoOnEdit );

function QLineEdit_create(parent: QWidgetH = nil): QLineEditH; overload; cdecl; external QtIntf name 'QLineEdit_create';
procedure QLineEdit_destroy(handle: QLineEditH); cdecl; external QtIntf name 'QLineEdit_destroy'; 
function QLineEdit_create(p1: PWideString; parent: QWidgetH = nil): QLineEditH; overload; cdecl; external QtIntf name 'QLineEdit_create2';
procedure QLineEdit_text(handle: QLineEditH; retval: PWideString); cdecl; external QtIntf name 'QLineEdit_text';
procedure QLineEdit_displayText(handle: QLineEditH; retval: PWideString); cdecl; external QtIntf name 'QLineEdit_displayText';
function QLineEdit_maxLength(handle: QLineEditH): Integer; cdecl; external QtIntf name 'QLineEdit_maxLength';
procedure QLineEdit_setMaxLength(handle: QLineEditH; p1: Integer); cdecl; external QtIntf name 'QLineEdit_setMaxLength';
procedure QLineEdit_setFrame(handle: QLineEditH; p1: Boolean); cdecl; external QtIntf name 'QLineEdit_setFrame';
function QLineEdit_hasFrame(handle: QLineEditH): Boolean; cdecl; external QtIntf name 'QLineEdit_hasFrame';
function QLineEdit_echoMode(handle: QLineEditH): QLineEditEchoMode; cdecl; external QtIntf name 'QLineEdit_echoMode';
procedure QLineEdit_setEchoMode(handle: QLineEditH; p1: QLineEditEchoMode); cdecl; external QtIntf name 'QLineEdit_setEchoMode';
function QLineEdit_isReadOnly(handle: QLineEditH): Boolean; cdecl; external QtIntf name 'QLineEdit_isReadOnly';
procedure QLineEdit_setReadOnly(handle: QLineEditH; p1: Boolean); cdecl; external QtIntf name 'QLineEdit_setReadOnly';
procedure QLineEdit_setValidator(handle: QLineEditH; p1: QValidatorH); cdecl; external QtIntf name 'QLineEdit_setValidator';
function QLineEdit_validator(handle: QLineEditH): QValidatorH; cdecl; external QtIntf name 'QLineEdit_validator';
procedure QLineEdit_setCompleter(handle: QLineEditH; completer: QCompleterH); cdecl; external QtIntf name 'QLineEdit_setCompleter';
function QLineEdit_completer(handle: QLineEditH): QCompleterH; cdecl; external QtIntf name 'QLineEdit_completer';
procedure QLineEdit_sizeHint(handle: QLineEditH; retval: PSize); cdecl; external QtIntf name 'QLineEdit_sizeHint';
procedure QLineEdit_minimumSizeHint(handle: QLineEditH; retval: PSize); cdecl; external QtIntf name 'QLineEdit_minimumSizeHint';
function QLineEdit_cursorPosition(handle: QLineEditH): Integer; cdecl; external QtIntf name 'QLineEdit_cursorPosition';
procedure QLineEdit_setCursorPosition(handle: QLineEditH; p1: Integer); cdecl; external QtIntf name 'QLineEdit_setCursorPosition';
function QLineEdit_cursorPositionAt(handle: QLineEditH; pos: PQtPoint): Integer; cdecl; external QtIntf name 'QLineEdit_cursorPositionAt';
procedure QLineEdit_setAlignment(handle: QLineEditH; flag: QtAlignment); cdecl; external QtIntf name 'QLineEdit_setAlignment';
function QLineEdit_alignment(handle: QLineEditH): QtAlignment; cdecl; external QtIntf name 'QLineEdit_alignment';
procedure QLineEdit_cursorForward(handle: QLineEditH; mark: Boolean; steps: Integer = 1); cdecl; external QtIntf name 'QLineEdit_cursorForward';
procedure QLineEdit_cursorBackward(handle: QLineEditH; mark: Boolean; steps: Integer = 1); cdecl; external QtIntf name 'QLineEdit_cursorBackward';
procedure QLineEdit_cursorWordForward(handle: QLineEditH; mark: Boolean); cdecl; external QtIntf name 'QLineEdit_cursorWordForward';
procedure QLineEdit_cursorWordBackward(handle: QLineEditH; mark: Boolean); cdecl; external QtIntf name 'QLineEdit_cursorWordBackward';
procedure QLineEdit_backspace(handle: QLineEditH); cdecl; external QtIntf name 'QLineEdit_backspace';
procedure QLineEdit_del(handle: QLineEditH); cdecl; external QtIntf name 'QLineEdit_del';
procedure QLineEdit_home(handle: QLineEditH; mark: Boolean); cdecl; external QtIntf name 'QLineEdit_home';
procedure QLineEdit_end(handle: QLineEditH; mark: Boolean); cdecl; external QtIntf name 'QLineEdit_end';
function QLineEdit_isModified(handle: QLineEditH): Boolean; cdecl; external QtIntf name 'QLineEdit_isModified';
procedure QLineEdit_setModified(handle: QLineEditH; p1: Boolean); cdecl; external QtIntf name 'QLineEdit_setModified';
procedure QLineEdit_setSelection(handle: QLineEditH; p1: Integer; p2: Integer); cdecl; external QtIntf name 'QLineEdit_setSelection';
function QLineEdit_hasSelectedText(handle: QLineEditH): Boolean; cdecl; external QtIntf name 'QLineEdit_hasSelectedText';
procedure QLineEdit_selectedText(handle: QLineEditH; retval: PWideString); cdecl; external QtIntf name 'QLineEdit_selectedText';
function QLineEdit_selectionStart(handle: QLineEditH): Integer; cdecl; external QtIntf name 'QLineEdit_selectionStart';
function QLineEdit_isUndoAvailable(handle: QLineEditH): Boolean; cdecl; external QtIntf name 'QLineEdit_isUndoAvailable';
function QLineEdit_isRedoAvailable(handle: QLineEditH): Boolean; cdecl; external QtIntf name 'QLineEdit_isRedoAvailable';
procedure QLineEdit_setDragEnabled(handle: QLineEditH; b: Boolean); cdecl; external QtIntf name 'QLineEdit_setDragEnabled';
function QLineEdit_dragEnabled(handle: QLineEditH): Boolean; cdecl; external QtIntf name 'QLineEdit_dragEnabled';
procedure QLineEdit_inputMask(handle: QLineEditH; retval: PWideString); cdecl; external QtIntf name 'QLineEdit_inputMask';
procedure QLineEdit_setInputMask(handle: QLineEditH; inputMask: PWideString); cdecl; external QtIntf name 'QLineEdit_setInputMask';
function QLineEdit_hasAcceptableInput(handle: QLineEditH): Boolean; cdecl; external QtIntf name 'QLineEdit_hasAcceptableInput';
procedure QLineEdit_setText(handle: QLineEditH; p1: PWideString); cdecl; external QtIntf name 'QLineEdit_setText';
procedure QLineEdit_clear(handle: QLineEditH); cdecl; external QtIntf name 'QLineEdit_clear';
procedure QLineEdit_selectAll(handle: QLineEditH); cdecl; external QtIntf name 'QLineEdit_selectAll';
procedure QLineEdit_undo(handle: QLineEditH); cdecl; external QtIntf name 'QLineEdit_undo';
procedure QLineEdit_redo(handle: QLineEditH); cdecl; external QtIntf name 'QLineEdit_redo';
procedure QLineEdit_cut(handle: QLineEditH); cdecl; external QtIntf name 'QLineEdit_cut';
procedure QLineEdit_copy(handle: QLineEditH); cdecl; external QtIntf name 'QLineEdit_copy';
procedure QLineEdit_paste(handle: QLineEditH); cdecl; external QtIntf name 'QLineEdit_paste';
procedure QLineEdit_deselect(handle: QLineEditH); cdecl; external QtIntf name 'QLineEdit_deselect';
procedure QLineEdit_insert(handle: QLineEditH; p1: PWideString); cdecl; external QtIntf name 'QLineEdit_insert';
function QLineEdit_createStandardContextMenu(handle: QLineEditH): QMenuH; cdecl; external QtIntf name 'QLineEdit_createStandardContextMenu';
procedure QLineEdit_inputMethodQuery(handle: QLineEditH; retval: QVariantH; p1: QtInputMethodQuery); cdecl; external QtIntf name 'QLineEdit_inputMethodQuery';
function QLineEdit_event(handle: QLineEditH; p1: QEventH): Boolean; cdecl; external QtIntf name 'QLineEdit_event';


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

type
  QTextEditAutoFormattingFlag = cardinal; // QTextEdit::AutoFormattingFlag
  QTextEditAutoFormatting = QTextEditAutoFormattingFlag; //QFlags<> (3)
const
  QTextEditAutoNone =   0;
  QTextEditAutoBulletList =   $00000001;
  QTextEditAutoAll =   $ffffffff;

function QTextEdit_create(parent: QWidgetH = nil): QTextEditH; overload; cdecl; external QtIntf name 'QTextEdit_create';
procedure QTextEdit_destroy(handle: QTextEditH); cdecl; external QtIntf name 'QTextEdit_destroy'; 
function QTextEdit_create(text: PWideString; parent: QWidgetH = nil): QTextEditH; overload; cdecl; external QtIntf name 'QTextEdit_create2';
procedure QTextEdit_setDocument(handle: QTextEditH; document: QTextDocumentH); cdecl; external QtIntf name 'QTextEdit_setDocument';
function QTextEdit_document(handle: QTextEditH): QTextDocumentH; cdecl; external QtIntf name 'QTextEdit_document';
procedure QTextEdit_setTextCursor(handle: QTextEditH; cursor: QTextCursorH); cdecl; external QtIntf name 'QTextEdit_setTextCursor';
procedure QTextEdit_textCursor(handle: QTextEditH; retval: QTextCursorH); cdecl; external QtIntf name 'QTextEdit_textCursor';
function QTextEdit_isReadOnly(handle: QTextEditH): Boolean; cdecl; external QtIntf name 'QTextEdit_isReadOnly';
procedure QTextEdit_setReadOnly(handle: QTextEditH; ro: Boolean); cdecl; external QtIntf name 'QTextEdit_setReadOnly';
procedure QTextEdit_setTextInteractionFlags(handle: QTextEditH; flags: QtTextInteractionFlags); cdecl; external QtIntf name 'QTextEdit_setTextInteractionFlags';
function QTextEdit_textInteractionFlags(handle: QTextEditH): QtTextInteractionFlags; cdecl; external QtIntf name 'QTextEdit_textInteractionFlags';
function QTextEdit_fontPointSize(handle: QTextEditH): Double; cdecl; external QtIntf name 'QTextEdit_fontPointSize';
procedure QTextEdit_fontFamily(handle: QTextEditH; retval: PWideString); cdecl; external QtIntf name 'QTextEdit_fontFamily';
function QTextEdit_fontWeight(handle: QTextEditH): Integer; cdecl; external QtIntf name 'QTextEdit_fontWeight';
function QTextEdit_fontUnderline(handle: QTextEditH): Boolean; cdecl; external QtIntf name 'QTextEdit_fontUnderline';
function QTextEdit_fontItalic(handle: QTextEditH): Boolean; cdecl; external QtIntf name 'QTextEdit_fontItalic';
procedure QTextEdit_textColor(handle: QTextEditH; retval: PQColor); cdecl; external QtIntf name 'QTextEdit_textColor';
procedure QTextEdit_currentFont(handle: QTextEditH; retval: QFontH); cdecl; external QtIntf name 'QTextEdit_currentFont';
function QTextEdit_alignment(handle: QTextEditH): QtAlignment; cdecl; external QtIntf name 'QTextEdit_alignment';
procedure QTextEdit_mergeCurrentCharFormat(handle: QTextEditH; modifier: QTextCharFormatH); cdecl; external QtIntf name 'QTextEdit_mergeCurrentCharFormat';
procedure QTextEdit_setCurrentCharFormat(handle: QTextEditH; format: QTextCharFormatH); cdecl; external QtIntf name 'QTextEdit_setCurrentCharFormat';
procedure QTextEdit_currentCharFormat(handle: QTextEditH; retval: QTextCharFormatH); cdecl; external QtIntf name 'QTextEdit_currentCharFormat';
function QTextEdit_autoFormatting(handle: QTextEditH): QTextEditAutoFormatting; cdecl; external QtIntf name 'QTextEdit_autoFormatting';
procedure QTextEdit_setAutoFormatting(handle: QTextEditH; features: QTextEditAutoFormatting); cdecl; external QtIntf name 'QTextEdit_setAutoFormatting';
function QTextEdit_tabChangesFocus(handle: QTextEditH): Boolean; cdecl; external QtIntf name 'QTextEdit_tabChangesFocus';
procedure QTextEdit_setTabChangesFocus(handle: QTextEditH; b: Boolean); cdecl; external QtIntf name 'QTextEdit_setTabChangesFocus';
procedure QTextEdit_setDocumentTitle(handle: QTextEditH; title: PWideString); cdecl; external QtIntf name 'QTextEdit_setDocumentTitle';
procedure QTextEdit_documentTitle(handle: QTextEditH; retval: PWideString); cdecl; external QtIntf name 'QTextEdit_documentTitle';
function QTextEdit_isUndoRedoEnabled(handle: QTextEditH): Boolean; cdecl; external QtIntf name 'QTextEdit_isUndoRedoEnabled';
procedure QTextEdit_setUndoRedoEnabled(handle: QTextEditH; enable: Boolean); cdecl; external QtIntf name 'QTextEdit_setUndoRedoEnabled';
function QTextEdit_lineWrapMode(handle: QTextEditH): QTextEditLineWrapMode; cdecl; external QtIntf name 'QTextEdit_lineWrapMode';
procedure QTextEdit_setLineWrapMode(handle: QTextEditH; mode: QTextEditLineWrapMode); cdecl; external QtIntf name 'QTextEdit_setLineWrapMode';
function QTextEdit_lineWrapColumnOrWidth(handle: QTextEditH): Integer; cdecl; external QtIntf name 'QTextEdit_lineWrapColumnOrWidth';
procedure QTextEdit_setLineWrapColumnOrWidth(handle: QTextEditH; w: Integer); cdecl; external QtIntf name 'QTextEdit_setLineWrapColumnOrWidth';
function QTextEdit_wordWrapMode(handle: QTextEditH): QTextOptionWrapMode; cdecl; external QtIntf name 'QTextEdit_wordWrapMode';
procedure QTextEdit_setWordWrapMode(handle: QTextEditH; policy: QTextOptionWrapMode); cdecl; external QtIntf name 'QTextEdit_setWordWrapMode';
function QTextEdit_find(handle: QTextEditH; exp: PWideString; options: QTextDocumentFindFlags = 0): Boolean; cdecl; external QtIntf name 'QTextEdit_find';
procedure QTextEdit_toPlainText(handle: QTextEditH; retval: PWideString); cdecl; external QtIntf name 'QTextEdit_toPlainText';
procedure QTextEdit_toHtml(handle: QTextEditH; retval: PWideString); cdecl; external QtIntf name 'QTextEdit_toHtml';
procedure QTextEdit_ensureCursorVisible(handle: QTextEditH); cdecl; external QtIntf name 'QTextEdit_ensureCursorVisible';
procedure QTextEdit_loadResource(handle: QTextEditH; retval: QVariantH; _type: Integer; name: QUrlH); cdecl; external QtIntf name 'QTextEdit_loadResource';
function QTextEdit_createStandardContextMenu(handle: QTextEditH): QMenuH; cdecl; external QtIntf name 'QTextEdit_createStandardContextMenu';
procedure QTextEdit_cursorForPosition(handle: QTextEditH; retval: QTextCursorH; pos: PQtPoint); cdecl; external QtIntf name 'QTextEdit_cursorForPosition';
procedure QTextEdit_cursorRect(handle: QTextEditH; retval: PRect; cursor: QTextCursorH); overload; cdecl; external QtIntf name 'QTextEdit_cursorRect';
procedure QTextEdit_cursorRect(handle: QTextEditH; retval: PRect); overload; cdecl; external QtIntf name 'QTextEdit_cursorRect2';
procedure QTextEdit_anchorAt(handle: QTextEditH; retval: PWideString; pos: PQtPoint); cdecl; external QtIntf name 'QTextEdit_anchorAt';
function QTextEdit_overwriteMode(handle: QTextEditH): Boolean; cdecl; external QtIntf name 'QTextEdit_overwriteMode';
procedure QTextEdit_setOverwriteMode(handle: QTextEditH; overwrite: Boolean); cdecl; external QtIntf name 'QTextEdit_setOverwriteMode';
function QTextEdit_tabStopWidth(handle: QTextEditH): Integer; cdecl; external QtIntf name 'QTextEdit_tabStopWidth';
procedure QTextEdit_setTabStopWidth(handle: QTextEditH; width: Integer); cdecl; external QtIntf name 'QTextEdit_setTabStopWidth';
function QTextEdit_cursorWidth(handle: QTextEditH): Integer; cdecl; external QtIntf name 'QTextEdit_cursorWidth';
procedure QTextEdit_setCursorWidth(handle: QTextEditH; width: Integer); cdecl; external QtIntf name 'QTextEdit_setCursorWidth';
function QTextEdit_acceptRichText(handle: QTextEditH): Boolean; cdecl; external QtIntf name 'QTextEdit_acceptRichText';
procedure QTextEdit_setAcceptRichText(handle: QTextEditH; accept: Boolean); cdecl; external QtIntf name 'QTextEdit_setAcceptRichText';
procedure QTextEdit_moveCursor(handle: QTextEditH; operation: QTextCursorMoveOperation; mode: QTextCursorMoveMode = QTextCursorMoveAnchor); cdecl; external QtIntf name 'QTextEdit_moveCursor';
function QTextEdit_canPaste(handle: QTextEditH): Boolean; cdecl; external QtIntf name 'QTextEdit_canPaste';
procedure QTextEdit_setFontPointSize(handle: QTextEditH; s: Double); cdecl; external QtIntf name 'QTextEdit_setFontPointSize';
procedure QTextEdit_setFontFamily(handle: QTextEditH; fontFamily: PWideString); cdecl; external QtIntf name 'QTextEdit_setFontFamily';
procedure QTextEdit_setFontWeight(handle: QTextEditH; w: Integer); cdecl; external QtIntf name 'QTextEdit_setFontWeight';
procedure QTextEdit_setFontUnderline(handle: QTextEditH; b: Boolean); cdecl; external QtIntf name 'QTextEdit_setFontUnderline';
procedure QTextEdit_setFontItalic(handle: QTextEditH; b: Boolean); cdecl; external QtIntf name 'QTextEdit_setFontItalic';
procedure QTextEdit_setTextColor(handle: QTextEditH; c: PQColor); cdecl; external QtIntf name 'QTextEdit_setTextColor';
procedure QTextEdit_setCurrentFont(handle: QTextEditH; f: QFontH); cdecl; external QtIntf name 'QTextEdit_setCurrentFont';
procedure QTextEdit_setAlignment(handle: QTextEditH; a: QtAlignment); cdecl; external QtIntf name 'QTextEdit_setAlignment';
procedure QTextEdit_setPlainText(handle: QTextEditH; text: PWideString); cdecl; external QtIntf name 'QTextEdit_setPlainText';
procedure QTextEdit_setHtml(handle: QTextEditH; text: PWideString); cdecl; external QtIntf name 'QTextEdit_setHtml';
procedure QTextEdit_setText(handle: QTextEditH; text: PWideString); cdecl; external QtIntf name 'QTextEdit_setText';
procedure QTextEdit_cut(handle: QTextEditH); cdecl; external QtIntf name 'QTextEdit_cut';
procedure QTextEdit_copy(handle: QTextEditH); cdecl; external QtIntf name 'QTextEdit_copy';
procedure QTextEdit_paste(handle: QTextEditH); cdecl; external QtIntf name 'QTextEdit_paste';
procedure QTextEdit_undo(handle: QTextEditH); cdecl; external QtIntf name 'QTextEdit_undo';
procedure QTextEdit_redo(handle: QTextEditH); cdecl; external QtIntf name 'QTextEdit_redo';
procedure QTextEdit_clear(handle: QTextEditH); cdecl; external QtIntf name 'QTextEdit_clear';
procedure QTextEdit_selectAll(handle: QTextEditH); cdecl; external QtIntf name 'QTextEdit_selectAll';
procedure QTextEdit_insertPlainText(handle: QTextEditH; text: PWideString); cdecl; external QtIntf name 'QTextEdit_insertPlainText';
procedure QTextEdit_insertHtml(handle: QTextEditH; text: PWideString); cdecl; external QtIntf name 'QTextEdit_insertHtml';
procedure QTextEdit_append(handle: QTextEditH; text: PWideString); cdecl; external QtIntf name 'QTextEdit_append';
procedure QTextEdit_scrollToAnchor(handle: QTextEditH; name: PWideString); cdecl; external QtIntf name 'QTextEdit_scrollToAnchor';
procedure QTextEdit_zoomIn(handle: QTextEditH; range: Integer = 1); cdecl; external QtIntf name 'QTextEdit_zoomIn';
procedure QTextEdit_zoomOut(handle: QTextEditH; range: Integer = 1); cdecl; external QtIntf name 'QTextEdit_zoomOut';


type
  QTextEdit_textChanged_Event = procedure () of object cdecl;
  QTextEdit_undoAvailable_Event = procedure (b: Boolean) of object cdecl;
  QTextEdit_redoAvailable_Event = procedure (b: Boolean) of object cdecl;
  QTextEdit_currentCharFormatChanged_Event = procedure (format: QTextCharFormatH) of object cdecl;
  QTextEdit_copyAvailable_Event = procedure (b: Boolean) of object cdecl;
  QTextEdit_selectionChanged_Event = procedure () of object cdecl;
  QTextEdit_cursorPositionChanged_Event = procedure () of object cdecl;


function QMainWindow_create(parent: QWidgetH = nil; flags: QtWindowFlags = 0): QMainWindowH; cdecl; external QtIntf name 'QMainWindow_create';
procedure QMainWindow_destroy(handle: QMainWindowH); cdecl; external QtIntf name 'QMainWindow_destroy'; 
procedure QMainWindow_iconSize(handle: QMainWindowH; retval: PSize); cdecl; external QtIntf name 'QMainWindow_iconSize';
procedure QMainWindow_setIconSize(handle: QMainWindowH; iconSize: PSize); cdecl; external QtIntf name 'QMainWindow_setIconSize';
function QMainWindow_toolButtonStyle(handle: QMainWindowH): QtToolButtonStyle; cdecl; external QtIntf name 'QMainWindow_toolButtonStyle';
procedure QMainWindow_setToolButtonStyle(handle: QMainWindowH; toolButtonStyle: QtToolButtonStyle); cdecl; external QtIntf name 'QMainWindow_setToolButtonStyle';
function QMainWindow_isAnimated(handle: QMainWindowH): Boolean; cdecl; external QtIntf name 'QMainWindow_isAnimated';
function QMainWindow_isDockNestingEnabled(handle: QMainWindowH): Boolean; cdecl; external QtIntf name 'QMainWindow_isDockNestingEnabled';
function QMainWindow_isSeparator(handle: QMainWindowH; pos: PQtPoint): Boolean; cdecl; external QtIntf name 'QMainWindow_isSeparator';
function QMainWindow_menuBar(handle: QMainWindowH): QMenuBarH; cdecl; external QtIntf name 'QMainWindow_menuBar';
procedure QMainWindow_setMenuBar(handle: QMainWindowH; menubar: QMenuBarH); cdecl; external QtIntf name 'QMainWindow_setMenuBar';
function QMainWindow_menuWidget(handle: QMainWindowH): QWidgetH; cdecl; external QtIntf name 'QMainWindow_menuWidget';
procedure QMainWindow_setMenuWidget(handle: QMainWindowH; menubar: QWidgetH); cdecl; external QtIntf name 'QMainWindow_setMenuWidget';
function QMainWindow_statusBar(handle: QMainWindowH): QStatusBarH; cdecl; external QtIntf name 'QMainWindow_statusBar';
procedure QMainWindow_setStatusBar(handle: QMainWindowH; statusbar: QStatusBarH); cdecl; external QtIntf name 'QMainWindow_setStatusBar';
function QMainWindow_centralWidget(handle: QMainWindowH): QWidgetH; cdecl; external QtIntf name 'QMainWindow_centralWidget';
procedure QMainWindow_setCentralWidget(handle: QMainWindowH; widget: QWidgetH); cdecl; external QtIntf name 'QMainWindow_setCentralWidget';
procedure QMainWindow_setCorner(handle: QMainWindowH; corner: QtCorner; area: QtDockWidgetArea); cdecl; external QtIntf name 'QMainWindow_setCorner';
function QMainWindow_corner(handle: QMainWindowH; corner: QtCorner): QtDockWidgetArea; cdecl; external QtIntf name 'QMainWindow_corner';
procedure QMainWindow_addToolBarBreak(handle: QMainWindowH; area: QtToolBarArea = QtTopToolBarArea); cdecl; external QtIntf name 'QMainWindow_addToolBarBreak';
procedure QMainWindow_insertToolBarBreak(handle: QMainWindowH; before: QToolBarH); cdecl; external QtIntf name 'QMainWindow_insertToolBarBreak';
procedure QMainWindow_addToolBar(handle: QMainWindowH; area: QtToolBarArea; toolbar: QToolBarH); overload; cdecl; external QtIntf name 'QMainWindow_addToolBar';
procedure QMainWindow_addToolBar(handle: QMainWindowH; toolbar: QToolBarH); overload; cdecl; external QtIntf name 'QMainWindow_addToolBar2';
function QMainWindow_addToolBar(handle: QMainWindowH; title: PWideString): QToolBarH; overload; cdecl; external QtIntf name 'QMainWindow_addToolBar3';
procedure QMainWindow_insertToolBar(handle: QMainWindowH; before: QToolBarH; toolbar: QToolBarH); cdecl; external QtIntf name 'QMainWindow_insertToolBar';
procedure QMainWindow_removeToolBar(handle: QMainWindowH; toolbar: QToolBarH); cdecl; external QtIntf name 'QMainWindow_removeToolBar';
function QMainWindow_toolBarArea(handle: QMainWindowH; toolbar: QToolBarH): QtToolBarArea; cdecl; external QtIntf name 'QMainWindow_toolBarArea';
procedure QMainWindow_addDockWidget(handle: QMainWindowH; area: QtDockWidgetArea; dockwidget: QDockWidgetH); overload; cdecl; external QtIntf name 'QMainWindow_addDockWidget';
procedure QMainWindow_addDockWidget(handle: QMainWindowH; area: QtDockWidgetArea; dockwidget: QDockWidgetH; orientation: QtOrientation); overload; cdecl; external QtIntf name 'QMainWindow_addDockWidget2';
procedure QMainWindow_splitDockWidget(handle: QMainWindowH; after: QDockWidgetH; dockwidget: QDockWidgetH; orientation: QtOrientation); cdecl; external QtIntf name 'QMainWindow_splitDockWidget';
procedure QMainWindow_tabifyDockWidget(handle: QMainWindowH; first: QDockWidgetH; second: QDockWidgetH); cdecl; external QtIntf name 'QMainWindow_tabifyDockWidget';
procedure QMainWindow_removeDockWidget(handle: QMainWindowH; dockwidget: QDockWidgetH); cdecl; external QtIntf name 'QMainWindow_removeDockWidget';
function QMainWindow_dockWidgetArea(handle: QMainWindowH; dockwidget: QDockWidgetH): QtDockWidgetArea; cdecl; external QtIntf name 'QMainWindow_dockWidgetArea';
procedure QMainWindow_saveState(handle: QMainWindowH; retval: QByteArrayH; version: Integer = 0); cdecl; external QtIntf name 'QMainWindow_saveState';
function QMainWindow_restoreState(handle: QMainWindowH; state: QByteArrayH; version: Integer = 0): Boolean; cdecl; external QtIntf name 'QMainWindow_restoreState';
function QMainWindow_createPopupMenu(handle: QMainWindowH): QMenuH; cdecl; external QtIntf name 'QMainWindow_createPopupMenu';
procedure QMainWindow_setAnimated(handle: QMainWindowH; enabled: Boolean); cdecl; external QtIntf name 'QMainWindow_setAnimated';
procedure QMainWindow_setDockNestingEnabled(handle: QMainWindowH; enabled: Boolean); cdecl; external QtIntf name 'QMainWindow_setDockNestingEnabled';


type
  QMainWindow_iconSizeChanged_Event = procedure (iconSize: PSize) of object cdecl;
  QMainWindow_toolButtonStyleChanged_Event = procedure (toolButtonStyle: QtToolButtonStyle) of object cdecl;


function QToolBar_create(title: PWideString; parent: QWidgetH = nil): QToolBarH; overload; cdecl; external QtIntf name 'QToolBar_create';
procedure QToolBar_destroy(handle: QToolBarH); cdecl; external QtIntf name 'QToolBar_destroy'; 
function QToolBar_create(parent: QWidgetH = nil): QToolBarH; overload; cdecl; external QtIntf name 'QToolBar_create2';
procedure QToolBar_setMovable(handle: QToolBarH; movable: Boolean); cdecl; external QtIntf name 'QToolBar_setMovable';
function QToolBar_isMovable(handle: QToolBarH): Boolean; cdecl; external QtIntf name 'QToolBar_isMovable';
procedure QToolBar_setAllowedAreas(handle: QToolBarH; areas: QtToolBarAreas); cdecl; external QtIntf name 'QToolBar_setAllowedAreas';
function QToolBar_allowedAreas(handle: QToolBarH): QtToolBarAreas; cdecl; external QtIntf name 'QToolBar_allowedAreas';
function QToolBar_isAreaAllowed(handle: QToolBarH; area: QtToolBarArea): Boolean; cdecl; external QtIntf name 'QToolBar_isAreaAllowed';
procedure QToolBar_setOrientation(handle: QToolBarH; orientation: QtOrientation); cdecl; external QtIntf name 'QToolBar_setOrientation';
function QToolBar_orientation(handle: QToolBarH): QtOrientation; cdecl; external QtIntf name 'QToolBar_orientation';
procedure QToolBar_clear(handle: QToolBarH); cdecl; external QtIntf name 'QToolBar_clear';
function QToolBar_addAction(handle: QToolBarH; text: PWideString): QActionH; overload; cdecl; external QtIntf name 'QToolBar_addAction';
function QToolBar_addAction(handle: QToolBarH; icon: QIconH; text: PWideString): QActionH; overload; cdecl; external QtIntf name 'QToolBar_addAction2';
function QToolBar_addAction(handle: QToolBarH; text: PWideString; receiver: QObjectH; member: PAnsiChar): QActionH; overload; cdecl; external QtIntf name 'QToolBar_addAction3';
function QToolBar_addAction(handle: QToolBarH; icon: QIconH; text: PWideString; receiver: QObjectH; member: PAnsiChar): QActionH; overload; cdecl; external QtIntf name 'QToolBar_addAction4';
function QToolBar_addSeparator(handle: QToolBarH): QActionH; cdecl; external QtIntf name 'QToolBar_addSeparator';
function QToolBar_insertSeparator(handle: QToolBarH; before: QActionH): QActionH; cdecl; external QtIntf name 'QToolBar_insertSeparator';
function QToolBar_addWidget(handle: QToolBarH; widget: QWidgetH): QActionH; cdecl; external QtIntf name 'QToolBar_addWidget';
function QToolBar_insertWidget(handle: QToolBarH; before: QActionH; widget: QWidgetH): QActionH; cdecl; external QtIntf name 'QToolBar_insertWidget';
procedure QToolBar_actionGeometry(handle: QToolBarH; retval: PRect; action: QActionH); cdecl; external QtIntf name 'QToolBar_actionGeometry';
function QToolBar_actionAt(handle: QToolBarH; p: PQtPoint): QActionH; overload; cdecl; external QtIntf name 'QToolBar_actionAt';
function QToolBar_actionAt(handle: QToolBarH; x: Integer; y: Integer): QActionH; overload; cdecl; external QtIntf name 'QToolBar_actionAt2';
function QToolBar_toggleViewAction(handle: QToolBarH): QActionH; cdecl; external QtIntf name 'QToolBar_toggleViewAction';
procedure QToolBar_iconSize(handle: QToolBarH; retval: PSize); cdecl; external QtIntf name 'QToolBar_iconSize';
function QToolBar_toolButtonStyle(handle: QToolBarH): QtToolButtonStyle; cdecl; external QtIntf name 'QToolBar_toolButtonStyle';
function QToolBar_widgetForAction(handle: QToolBarH; action: QActionH): QWidgetH; cdecl; external QtIntf name 'QToolBar_widgetForAction';
procedure QToolBar_setIconSize(handle: QToolBarH; iconSize: PSize); cdecl; external QtIntf name 'QToolBar_setIconSize';
procedure QToolBar_setToolButtonStyle(handle: QToolBarH; toolButtonStyle: QtToolButtonStyle); cdecl; external QtIntf name 'QToolBar_setToolButtonStyle';


type
  QToolBar_actionTriggered_Event = procedure (action: QActionH) of object cdecl;
  QToolBar_movableChanged_Event = procedure (movable: Boolean) of object cdecl;
  QToolBar_allowedAreasChanged_Event = procedure (allowedAreas: QtToolBarAreas) of object cdecl;
  QToolBar_orientationChanged_Event = procedure (orientation: QtOrientation) of object cdecl;
  QToolBar_iconSizeChanged_Event = procedure (iconSize: PSize) of object cdecl;
  QToolBar_toolButtonStyleChanged_Event = procedure (toolButtonStyle: QtToolButtonStyle) of object cdecl;


function QSizeGrip_create(parent: QWidgetH): QSizeGripH; cdecl; external QtIntf name 'QSizeGrip_create';
procedure QSizeGrip_destroy(handle: QSizeGripH); cdecl; external QtIntf name 'QSizeGrip_destroy'; 
procedure QSizeGrip_sizeHint(handle: QSizeGripH; retval: PSize); cdecl; external QtIntf name 'QSizeGrip_sizeHint';
procedure QSizeGrip_setVisible(handle: QSizeGripH; p1: Boolean); cdecl; external QtIntf name 'QSizeGrip_setVisible';


type
  QLCDNumberMode = ( // QLCDNumber::Mode (1)
    QLCDNumberHex, QLCDNumberDec, QLCDNumberOct, QLCDNumberBin );

  QLCDNumberSegmentStyle = ( // QLCDNumber::SegmentStyle (1)
    QLCDNumberOutline, QLCDNumberFilled, QLCDNumberFlat );

function QLCDNumber_create(parent: QWidgetH = nil): QLCDNumberH; overload; cdecl; external QtIntf name 'QLCDNumber_create';
procedure QLCDNumber_destroy(handle: QLCDNumberH); cdecl; external QtIntf name 'QLCDNumber_destroy'; 
function QLCDNumber_create(numDigits: LongWord; parent: QWidgetH = nil): QLCDNumberH; overload; cdecl; external QtIntf name 'QLCDNumber_create2';
function QLCDNumber_smallDecimalPoint(handle: QLCDNumberH): Boolean; cdecl; external QtIntf name 'QLCDNumber_smallDecimalPoint';
function QLCDNumber_numDigits(handle: QLCDNumberH): Integer; cdecl; external QtIntf name 'QLCDNumber_numDigits';
procedure QLCDNumber_setNumDigits(handle: QLCDNumberH; nDigits: Integer); cdecl; external QtIntf name 'QLCDNumber_setNumDigits';
function QLCDNumber_checkOverflow(handle: QLCDNumberH; num: Double): Boolean; overload; cdecl; external QtIntf name 'QLCDNumber_checkOverflow';
function QLCDNumber_checkOverflow(handle: QLCDNumberH; num: Integer): Boolean; overload; cdecl; external QtIntf name 'QLCDNumber_checkOverflow2';
function QLCDNumber_mode(handle: QLCDNumberH): QLCDNumberMode; cdecl; external QtIntf name 'QLCDNumber_mode';
procedure QLCDNumber_setMode(handle: QLCDNumberH; p1: QLCDNumberMode); cdecl; external QtIntf name 'QLCDNumber_setMode';
function QLCDNumber_segmentStyle(handle: QLCDNumberH): QLCDNumberSegmentStyle; cdecl; external QtIntf name 'QLCDNumber_segmentStyle';
procedure QLCDNumber_setSegmentStyle(handle: QLCDNumberH; p1: QLCDNumberSegmentStyle); cdecl; external QtIntf name 'QLCDNumber_setSegmentStyle';
function QLCDNumber_value(handle: QLCDNumberH): Double; cdecl; external QtIntf name 'QLCDNumber_value';
function QLCDNumber_intValue(handle: QLCDNumberH): Integer; cdecl; external QtIntf name 'QLCDNumber_intValue';
procedure QLCDNumber_sizeHint(handle: QLCDNumberH; retval: PSize); cdecl; external QtIntf name 'QLCDNumber_sizeHint';
procedure QLCDNumber_display(handle: QLCDNumberH; str: PWideString); overload; cdecl; external QtIntf name 'QLCDNumber_display';
procedure QLCDNumber_display(handle: QLCDNumberH; num: Integer); overload; cdecl; external QtIntf name 'QLCDNumber_display2';
procedure QLCDNumber_display(handle: QLCDNumberH; num: Double); overload; cdecl; external QtIntf name 'QLCDNumber_display3';
procedure QLCDNumber_setHexMode(handle: QLCDNumberH); cdecl; external QtIntf name 'QLCDNumber_setHexMode';
procedure QLCDNumber_setDecMode(handle: QLCDNumberH); cdecl; external QtIntf name 'QLCDNumber_setDecMode';
procedure QLCDNumber_setOctMode(handle: QLCDNumberH); cdecl; external QtIntf name 'QLCDNumber_setOctMode';
procedure QLCDNumber_setBinMode(handle: QLCDNumberH); cdecl; external QtIntf name 'QLCDNumber_setBinMode';
procedure QLCDNumber_setSmallDecimalPoint(handle: QLCDNumberH; p1: Boolean); cdecl; external QtIntf name 'QLCDNumber_setSmallDecimalPoint';


type
  QLCDNumber_overflow_Event = procedure () of object cdecl;



type
  QAbstractSpinBoxButtonSymbols = ( // QAbstractSpinBox::ButtonSymbols (1)
    QAbstractSpinBoxUpDownArrows, QAbstractSpinBoxPlusMinus );

  QAbstractSpinBoxCorrectionMode = ( // QAbstractSpinBox::CorrectionMode (1)
    QAbstractSpinBoxCorrectToPreviousValue, QAbstractSpinBoxCorrectToNearestValue );

type
  QAbstractSpinBoxStepEnabledFlag = cardinal; // QAbstractSpinBox::StepEnabledFlag
  QAbstractSpinBoxStepEnabled = QAbstractSpinBoxStepEnabledFlag; //QFlags<> (3)
const
  QAbstractSpinBoxStepNone =   $00;
  QAbstractSpinBoxStepUpEnabled =   $01;
  QAbstractSpinBoxStepDownEnabled =   $02;

function QAbstractSpinBox_create(parent: QWidgetH = nil): QAbstractSpinBoxH; cdecl; external QtIntf name 'QAbstractSpinBox_create';
procedure QAbstractSpinBox_destroy(handle: QAbstractSpinBoxH); cdecl; external QtIntf name 'QAbstractSpinBox_destroy'; 
function QAbstractSpinBox_buttonSymbols(handle: QAbstractSpinBoxH): QAbstractSpinBoxButtonSymbols; cdecl; external QtIntf name 'QAbstractSpinBox_buttonSymbols';
procedure QAbstractSpinBox_setButtonSymbols(handle: QAbstractSpinBoxH; bs: QAbstractSpinBoxButtonSymbols); cdecl; external QtIntf name 'QAbstractSpinBox_setButtonSymbols';
procedure QAbstractSpinBox_setCorrectionMode(handle: QAbstractSpinBoxH; cm: QAbstractSpinBoxCorrectionMode); cdecl; external QtIntf name 'QAbstractSpinBox_setCorrectionMode';
function QAbstractSpinBox_correctionMode(handle: QAbstractSpinBoxH): QAbstractSpinBoxCorrectionMode; cdecl; external QtIntf name 'QAbstractSpinBox_correctionMode';
function QAbstractSpinBox_hasAcceptableInput(handle: QAbstractSpinBoxH): Boolean; cdecl; external QtIntf name 'QAbstractSpinBox_hasAcceptableInput';
procedure QAbstractSpinBox_text(handle: QAbstractSpinBoxH; retval: PWideString); cdecl; external QtIntf name 'QAbstractSpinBox_text';
procedure QAbstractSpinBox_specialValueText(handle: QAbstractSpinBoxH; retval: PWideString); cdecl; external QtIntf name 'QAbstractSpinBox_specialValueText';
procedure QAbstractSpinBox_setSpecialValueText(handle: QAbstractSpinBoxH; txt: PWideString); cdecl; external QtIntf name 'QAbstractSpinBox_setSpecialValueText';
function QAbstractSpinBox_wrapping(handle: QAbstractSpinBoxH): Boolean; cdecl; external QtIntf name 'QAbstractSpinBox_wrapping';
procedure QAbstractSpinBox_setWrapping(handle: QAbstractSpinBoxH; w: Boolean); cdecl; external QtIntf name 'QAbstractSpinBox_setWrapping';
procedure QAbstractSpinBox_setReadOnly(handle: QAbstractSpinBoxH; r: Boolean); cdecl; external QtIntf name 'QAbstractSpinBox_setReadOnly';
function QAbstractSpinBox_isReadOnly(handle: QAbstractSpinBoxH): Boolean; cdecl; external QtIntf name 'QAbstractSpinBox_isReadOnly';
procedure QAbstractSpinBox_setAlignment(handle: QAbstractSpinBoxH; flag: QtAlignment); cdecl; external QtIntf name 'QAbstractSpinBox_setAlignment';
function QAbstractSpinBox_alignment(handle: QAbstractSpinBoxH): QtAlignment; cdecl; external QtIntf name 'QAbstractSpinBox_alignment';
procedure QAbstractSpinBox_setFrame(handle: QAbstractSpinBoxH; p1: Boolean); cdecl; external QtIntf name 'QAbstractSpinBox_setFrame';
function QAbstractSpinBox_hasFrame(handle: QAbstractSpinBoxH): Boolean; cdecl; external QtIntf name 'QAbstractSpinBox_hasFrame';
procedure QAbstractSpinBox_setAccelerated(handle: QAbstractSpinBoxH; _on: Boolean); cdecl; external QtIntf name 'QAbstractSpinBox_setAccelerated';
function QAbstractSpinBox_isAccelerated(handle: QAbstractSpinBoxH): Boolean; cdecl; external QtIntf name 'QAbstractSpinBox_isAccelerated';
procedure QAbstractSpinBox_sizeHint(handle: QAbstractSpinBoxH; retval: PSize); cdecl; external QtIntf name 'QAbstractSpinBox_sizeHint';
procedure QAbstractSpinBox_minimumSizeHint(handle: QAbstractSpinBoxH; retval: PSize); cdecl; external QtIntf name 'QAbstractSpinBox_minimumSizeHint';
procedure QAbstractSpinBox_interpretText(handle: QAbstractSpinBoxH); cdecl; external QtIntf name 'QAbstractSpinBox_interpretText';
function QAbstractSpinBox_event(handle: QAbstractSpinBoxH; event: QEventH): Boolean; cdecl; external QtIntf name 'QAbstractSpinBox_event';
function QAbstractSpinBox_validate(handle: QAbstractSpinBoxH; input: PWideString; pos: PInteger): QValidatorState; cdecl; external QtIntf name 'QAbstractSpinBox_validate';
procedure QAbstractSpinBox_fixup(handle: QAbstractSpinBoxH; input: PWideString); cdecl; external QtIntf name 'QAbstractSpinBox_fixup';
procedure QAbstractSpinBox_stepBy(handle: QAbstractSpinBoxH; steps: Integer); cdecl; external QtIntf name 'QAbstractSpinBox_stepBy';
procedure QAbstractSpinBox_stepUp(handle: QAbstractSpinBoxH); cdecl; external QtIntf name 'QAbstractSpinBox_stepUp';
procedure QAbstractSpinBox_stepDown(handle: QAbstractSpinBoxH); cdecl; external QtIntf name 'QAbstractSpinBox_stepDown';
procedure QAbstractSpinBox_selectAll(handle: QAbstractSpinBoxH); cdecl; external QtIntf name 'QAbstractSpinBox_selectAll';
procedure QAbstractSpinBox_clear(handle: QAbstractSpinBoxH); cdecl; external QtIntf name 'QAbstractSpinBox_clear';


type
  QAbstractSpinBox_editingFinished_Event = procedure () of object cdecl;


function QLCLAbstractSpinBox_lineEditHandle(protectedhandle: QAbstractSpinBoxH): QLineEditH; cdecl; external QtIntf name 'QLCLAbstractSpinBox_lineEditHandle';

function QSpinBox_create(parent: QWidgetH = nil): QSpinBoxH; cdecl; external QtIntf name 'QSpinBox_create';
procedure QSpinBox_destroy(handle: QSpinBoxH); cdecl; external QtIntf name 'QSpinBox_destroy'; 
function QSpinBox_value(handle: QSpinBoxH): Integer; cdecl; external QtIntf name 'QSpinBox_value';
procedure QSpinBox_prefix(handle: QSpinBoxH; retval: PWideString); cdecl; external QtIntf name 'QSpinBox_prefix';
procedure QSpinBox_setPrefix(handle: QSpinBoxH; prefix: PWideString); cdecl; external QtIntf name 'QSpinBox_setPrefix';
procedure QSpinBox_suffix(handle: QSpinBoxH; retval: PWideString); cdecl; external QtIntf name 'QSpinBox_suffix';
procedure QSpinBox_setSuffix(handle: QSpinBoxH; suffix: PWideString); cdecl; external QtIntf name 'QSpinBox_setSuffix';
procedure QSpinBox_cleanText(handle: QSpinBoxH; retval: PWideString); cdecl; external QtIntf name 'QSpinBox_cleanText';
function QSpinBox_singleStep(handle: QSpinBoxH): Integer; cdecl; external QtIntf name 'QSpinBox_singleStep';
procedure QSpinBox_setSingleStep(handle: QSpinBoxH; val: Integer); cdecl; external QtIntf name 'QSpinBox_setSingleStep';
function QSpinBox_minimum(handle: QSpinBoxH): Integer; cdecl; external QtIntf name 'QSpinBox_minimum';
procedure QSpinBox_setMinimum(handle: QSpinBoxH; min: Integer); cdecl; external QtIntf name 'QSpinBox_setMinimum';
function QSpinBox_maximum(handle: QSpinBoxH): Integer; cdecl; external QtIntf name 'QSpinBox_maximum';
procedure QSpinBox_setMaximum(handle: QSpinBoxH; max: Integer); cdecl; external QtIntf name 'QSpinBox_setMaximum';
procedure QSpinBox_setRange(handle: QSpinBoxH; min: Integer; max: Integer); cdecl; external QtIntf name 'QSpinBox_setRange';
procedure QSpinBox_setValue(handle: QSpinBoxH; val: Integer); cdecl; external QtIntf name 'QSpinBox_setValue';

function QDoubleSpinBox_create(parent: QWidgetH = nil): QDoubleSpinBoxH; cdecl; external QtIntf name 'QDoubleSpinBox_create';
procedure QDoubleSpinBox_destroy(handle: QDoubleSpinBoxH); cdecl; external QtIntf name 'QDoubleSpinBox_destroy'; 
function QDoubleSpinBox_value(handle: QDoubleSpinBoxH): Double; cdecl; external QtIntf name 'QDoubleSpinBox_value';
procedure QDoubleSpinBox_prefix(handle: QDoubleSpinBoxH; retval: PWideString); cdecl; external QtIntf name 'QDoubleSpinBox_prefix';
procedure QDoubleSpinBox_setPrefix(handle: QDoubleSpinBoxH; prefix: PWideString); cdecl; external QtIntf name 'QDoubleSpinBox_setPrefix';
procedure QDoubleSpinBox_suffix(handle: QDoubleSpinBoxH; retval: PWideString); cdecl; external QtIntf name 'QDoubleSpinBox_suffix';
procedure QDoubleSpinBox_setSuffix(handle: QDoubleSpinBoxH; suffix: PWideString); cdecl; external QtIntf name 'QDoubleSpinBox_setSuffix';
procedure QDoubleSpinBox_cleanText(handle: QDoubleSpinBoxH; retval: PWideString); cdecl; external QtIntf name 'QDoubleSpinBox_cleanText';
function QDoubleSpinBox_singleStep(handle: QDoubleSpinBoxH): Double; cdecl; external QtIntf name 'QDoubleSpinBox_singleStep';
procedure QDoubleSpinBox_setSingleStep(handle: QDoubleSpinBoxH; val: Double); cdecl; external QtIntf name 'QDoubleSpinBox_setSingleStep';
function QDoubleSpinBox_minimum(handle: QDoubleSpinBoxH): Double; cdecl; external QtIntf name 'QDoubleSpinBox_minimum';
procedure QDoubleSpinBox_setMinimum(handle: QDoubleSpinBoxH; min: Double); cdecl; external QtIntf name 'QDoubleSpinBox_setMinimum';
function QDoubleSpinBox_maximum(handle: QDoubleSpinBoxH): Double; cdecl; external QtIntf name 'QDoubleSpinBox_maximum';
procedure QDoubleSpinBox_setMaximum(handle: QDoubleSpinBoxH; max: Double); cdecl; external QtIntf name 'QDoubleSpinBox_setMaximum';
procedure QDoubleSpinBox_setRange(handle: QDoubleSpinBoxH; min: Double; max: Double); cdecl; external QtIntf name 'QDoubleSpinBox_setRange';
function QDoubleSpinBox_decimals(handle: QDoubleSpinBoxH): Integer; cdecl; external QtIntf name 'QDoubleSpinBox_decimals';
procedure QDoubleSpinBox_setDecimals(handle: QDoubleSpinBoxH; prec: Integer); cdecl; external QtIntf name 'QDoubleSpinBox_setDecimals';
function QDoubleSpinBox_validate(handle: QDoubleSpinBoxH; input: PWideString; pos: PInteger): QValidatorState; cdecl; external QtIntf name 'QDoubleSpinBox_validate';
function QDoubleSpinBox_valueFromText(handle: QDoubleSpinBoxH; text: PWideString): Double; cdecl; external QtIntf name 'QDoubleSpinBox_valueFromText';
procedure QDoubleSpinBox_textFromValue(handle: QDoubleSpinBoxH; retval: PWideString; val: Double); cdecl; external QtIntf name 'QDoubleSpinBox_textFromValue';
procedure QDoubleSpinBox_fixup(handle: QDoubleSpinBoxH; str: PWideString); cdecl; external QtIntf name 'QDoubleSpinBox_fixup';
procedure QDoubleSpinBox_setValue(handle: QDoubleSpinBoxH; val: Double); cdecl; external QtIntf name 'QDoubleSpinBox_setValue';


type
  QSpinBox_valueChanged_Event = procedure (p1: Integer) of object cdecl;
  QSpinBox_valueChanged2_Event = procedure (p1: PWideString) of object cdecl;



type
  QDoubleSpinBox_valueChanged_Event = procedure (p1: Double) of object cdecl;
  QDoubleSpinBox_valueChanged2_Event = procedure (p1: PWideString) of object cdecl;


function QSplitter_create(parent: QWidgetH = nil): QSplitterH; overload; cdecl; external QtIntf name 'QSplitter_create';
procedure QSplitter_destroy(handle: QSplitterH); cdecl; external QtIntf name 'QSplitter_destroy'; 
function QSplitter_create(p1: QtOrientation; parent: QWidgetH = nil): QSplitterH; overload; cdecl; external QtIntf name 'QSplitter_create2';
procedure QSplitter_addWidget(handle: QSplitterH; widget: QWidgetH); cdecl; external QtIntf name 'QSplitter_addWidget';
procedure QSplitter_insertWidget(handle: QSplitterH; index: Integer; widget: QWidgetH); cdecl; external QtIntf name 'QSplitter_insertWidget';
procedure QSplitter_setOrientation(handle: QSplitterH; p1: QtOrientation); cdecl; external QtIntf name 'QSplitter_setOrientation';
function QSplitter_orientation(handle: QSplitterH): QtOrientation; cdecl; external QtIntf name 'QSplitter_orientation';
procedure QSplitter_setChildrenCollapsible(handle: QSplitterH; p1: Boolean); cdecl; external QtIntf name 'QSplitter_setChildrenCollapsible';
function QSplitter_childrenCollapsible(handle: QSplitterH): Boolean; cdecl; external QtIntf name 'QSplitter_childrenCollapsible';
procedure QSplitter_setCollapsible(handle: QSplitterH; index: Integer; p2: Boolean); cdecl; external QtIntf name 'QSplitter_setCollapsible';
function QSplitter_isCollapsible(handle: QSplitterH; index: Integer): Boolean; cdecl; external QtIntf name 'QSplitter_isCollapsible';
procedure QSplitter_setOpaqueResize(handle: QSplitterH; opaque: Boolean = True); cdecl; external QtIntf name 'QSplitter_setOpaqueResize';
function QSplitter_opaqueResize(handle: QSplitterH): Boolean; cdecl; external QtIntf name 'QSplitter_opaqueResize';
procedure QSplitter_refresh(handle: QSplitterH); cdecl; external QtIntf name 'QSplitter_refresh';
procedure QSplitter_sizeHint(handle: QSplitterH; retval: PSize); cdecl; external QtIntf name 'QSplitter_sizeHint';
procedure QSplitter_minimumSizeHint(handle: QSplitterH; retval: PSize); cdecl; external QtIntf name 'QSplitter_minimumSizeHint';
procedure QSplitter_sizes(handle: QSplitterH; retval: PIntArray); cdecl; external QtIntf name 'QSplitter_sizes';
procedure QSplitter_setSizes(handle: QSplitterH; list: PIntArray); cdecl; external QtIntf name 'QSplitter_setSizes';
procedure QSplitter_saveState(handle: QSplitterH; retval: QByteArrayH); cdecl; external QtIntf name 'QSplitter_saveState';
function QSplitter_restoreState(handle: QSplitterH; state: QByteArrayH): Boolean; cdecl; external QtIntf name 'QSplitter_restoreState';
function QSplitter_handleWidth(handle: QSplitterH): Integer; cdecl; external QtIntf name 'QSplitter_handleWidth';
procedure QSplitter_setHandleWidth(handle: QSplitterH; p1: Integer); cdecl; external QtIntf name 'QSplitter_setHandleWidth';
function QSplitter_indexOf(handle: QSplitterH; w: QWidgetH): Integer; cdecl; external QtIntf name 'QSplitter_indexOf';
function QSplitter_widget(handle: QSplitterH; index: Integer): QWidgetH; cdecl; external QtIntf name 'QSplitter_widget';
function QSplitter_count(handle: QSplitterH): Integer; cdecl; external QtIntf name 'QSplitter_count';
procedure QSplitter_getRange(handle: QSplitterH; index: Integer; p2: PInteger; p3: PInteger); cdecl; external QtIntf name 'QSplitter_getRange';
function QSplitter_handle(handle: QSplitterH; index: Integer): QSplitterHandleH; cdecl; external QtIntf name 'QSplitter_handle';
procedure QSplitter_setStretchFactor(handle: QSplitterH; index: Integer; stretch: Integer); cdecl; external QtIntf name 'QSplitter_setStretchFactor';

function QSplitterHandle_create(o: QtOrientation; parent: QSplitterH): QSplitterHandleH; cdecl; external QtIntf name 'QSplitterHandle_create';
procedure QSplitterHandle_destroy(handle: QSplitterHandleH); cdecl; external QtIntf name 'QSplitterHandle_destroy'; 
procedure QSplitterHandle_setOrientation(handle: QSplitterHandleH; o: QtOrientation); cdecl; external QtIntf name 'QSplitterHandle_setOrientation';
function QSplitterHandle_orientation(handle: QSplitterHandleH): QtOrientation; cdecl; external QtIntf name 'QSplitterHandle_orientation';
function QSplitterHandle_opaqueResize(handle: QSplitterHandleH): Boolean; cdecl; external QtIntf name 'QSplitterHandle_opaqueResize';
function QSplitterHandle_splitter(handle: QSplitterHandleH): QSplitterH; cdecl; external QtIntf name 'QSplitterHandle_splitter';
procedure QSplitterHandle_sizeHint(handle: QSplitterHandleH; retval: PSize); cdecl; external QtIntf name 'QSplitterHandle_sizeHint';


type
  QSplitter_splitterMoved_Event = procedure (pos: Integer; index: Integer) of object cdecl;



type
  QWorkspaceWindowOrder = ( // QWorkspace::WindowOrder (1)
    QWorkspaceCreationOrder, QWorkspaceStackingOrder );

function QWorkspace_create(parent: QWidgetH = nil): QWorkspaceH; cdecl; external QtIntf name 'QWorkspace_create';
procedure QWorkspace_destroy(handle: QWorkspaceH); cdecl; external QtIntf name 'QWorkspace_destroy'; 
function QWorkspace_activeWindow(handle: QWorkspaceH): QWidgetH; cdecl; external QtIntf name 'QWorkspace_activeWindow';
procedure QWorkspace_windowList(handle: QWorkspaceH; retval: PIntArray; order: QWorkspaceWindowOrder = QWorkspaceCreationOrder); cdecl; external QtIntf name 'QWorkspace_windowList';
function QWorkspace_addWindow(handle: QWorkspaceH; w: QWidgetH; flags: QtWindowFlags = 0): QWidgetH; cdecl; external QtIntf name 'QWorkspace_addWindow';
procedure QWorkspace_sizeHint(handle: QWorkspaceH; retval: PSize); cdecl; external QtIntf name 'QWorkspace_sizeHint';
function QWorkspace_scrollBarsEnabled(handle: QWorkspaceH): Boolean; cdecl; external QtIntf name 'QWorkspace_scrollBarsEnabled';
procedure QWorkspace_setScrollBarsEnabled(handle: QWorkspaceH; enable: Boolean); cdecl; external QtIntf name 'QWorkspace_setScrollBarsEnabled';
procedure QWorkspace_setBackground(handle: QWorkspaceH; background: QBrushH); cdecl; external QtIntf name 'QWorkspace_setBackground';
procedure QWorkspace_background(handle: QWorkspaceH; retval: QBrushH); cdecl; external QtIntf name 'QWorkspace_background';
procedure QWorkspace_setActiveWindow(handle: QWorkspaceH; w: QWidgetH); cdecl; external QtIntf name 'QWorkspace_setActiveWindow';
procedure QWorkspace_cascade(handle: QWorkspaceH); cdecl; external QtIntf name 'QWorkspace_cascade';
procedure QWorkspace_tile(handle: QWorkspaceH); cdecl; external QtIntf name 'QWorkspace_tile';
procedure QWorkspace_arrangeIcons(handle: QWorkspaceH); cdecl; external QtIntf name 'QWorkspace_arrangeIcons';
procedure QWorkspace_closeActiveWindow(handle: QWorkspaceH); cdecl; external QtIntf name 'QWorkspace_closeActiveWindow';
procedure QWorkspace_closeAllWindows(handle: QWorkspaceH); cdecl; external QtIntf name 'QWorkspace_closeAllWindows';
procedure QWorkspace_activateNextWindow(handle: QWorkspaceH); cdecl; external QtIntf name 'QWorkspace_activateNextWindow';
procedure QWorkspace_activatePreviousWindow(handle: QWorkspaceH); cdecl; external QtIntf name 'QWorkspace_activatePreviousWindow';


type
  QWorkspace_windowActivated_Event = procedure (w: QWidgetH) of object cdecl;



type
  QComboBoxInsertPolicy = ( // QComboBox::InsertPolicy (1)
    QComboBoxNoInsert, QComboBoxInsertAtTop, QComboBoxInsertAtCurrent, QComboBoxInsertAtBottom, QComboBoxInsertAfterCurrent, QComboBoxInsertBeforeCurrent, QComboBoxInsertAlphabetically );

  QComboBoxSizeAdjustPolicy = ( // QComboBox::SizeAdjustPolicy (1)
    QComboBoxAdjustToContents, QComboBoxAdjustToContentsOnFirstShow, QComboBoxAdjustToMinimumContentsLength );

function QComboBox_create(parent: QWidgetH = nil): QComboBoxH; cdecl; external QtIntf name 'QComboBox_create';
procedure QComboBox_destroy(handle: QComboBoxH); cdecl; external QtIntf name 'QComboBox_destroy'; 
function QComboBox_maxVisibleItems(handle: QComboBoxH): Integer; cdecl; external QtIntf name 'QComboBox_maxVisibleItems';
procedure QComboBox_setMaxVisibleItems(handle: QComboBoxH; maxItems: Integer); cdecl; external QtIntf name 'QComboBox_setMaxVisibleItems';
function QComboBox_count(handle: QComboBoxH): Integer; cdecl; external QtIntf name 'QComboBox_count';
procedure QComboBox_setMaxCount(handle: QComboBoxH; max: Integer); cdecl; external QtIntf name 'QComboBox_setMaxCount';
function QComboBox_maxCount(handle: QComboBoxH): Integer; cdecl; external QtIntf name 'QComboBox_maxCount';
function QComboBox_autoCompletion(handle: QComboBoxH): Boolean; cdecl; external QtIntf name 'QComboBox_autoCompletion';
procedure QComboBox_setAutoCompletion(handle: QComboBoxH; enable: Boolean); cdecl; external QtIntf name 'QComboBox_setAutoCompletion';
function QComboBox_autoCompletionCaseSensitivity(handle: QComboBoxH): QtCaseSensitivity; cdecl; external QtIntf name 'QComboBox_autoCompletionCaseSensitivity';
procedure QComboBox_setAutoCompletionCaseSensitivity(handle: QComboBoxH; sensitivity: QtCaseSensitivity); cdecl; external QtIntf name 'QComboBox_setAutoCompletionCaseSensitivity';
function QComboBox_duplicatesEnabled(handle: QComboBoxH): Boolean; cdecl; external QtIntf name 'QComboBox_duplicatesEnabled';
procedure QComboBox_setDuplicatesEnabled(handle: QComboBoxH; enable: Boolean); cdecl; external QtIntf name 'QComboBox_setDuplicatesEnabled';
procedure QComboBox_setFrame(handle: QComboBoxH; p1: Boolean); cdecl; external QtIntf name 'QComboBox_setFrame';
function QComboBox_hasFrame(handle: QComboBoxH): Boolean; cdecl; external QtIntf name 'QComboBox_hasFrame';
function QComboBox_findText(handle: QComboBoxH; text: PWideString; flags: QtMatchFlags = QtMatchExactly or QtMatchCaseSensitive): Integer; cdecl; external QtIntf name 'QComboBox_findText';
function QComboBox_findData(handle: QComboBoxH; data: QVariantH; role: QtItemDataRole = QtUserRole; flags: QtMatchFlags = QtMatchExactly or QtMatchCaseSensitive): Integer; cdecl; external QtIntf name 'QComboBox_findData';
function QComboBox_insertPolicy(handle: QComboBoxH): QComboBoxInsertPolicy; cdecl; external QtIntf name 'QComboBox_insertPolicy';
procedure QComboBox_setInsertPolicy(handle: QComboBoxH; policy: QComboBoxInsertPolicy); cdecl; external QtIntf name 'QComboBox_setInsertPolicy';
function QComboBox_sizeAdjustPolicy(handle: QComboBoxH): QComboBoxSizeAdjustPolicy; cdecl; external QtIntf name 'QComboBox_sizeAdjustPolicy';
procedure QComboBox_setSizeAdjustPolicy(handle: QComboBoxH; policy: QComboBoxSizeAdjustPolicy); cdecl; external QtIntf name 'QComboBox_setSizeAdjustPolicy';
function QComboBox_minimumContentsLength(handle: QComboBoxH): Integer; cdecl; external QtIntf name 'QComboBox_minimumContentsLength';
procedure QComboBox_setMinimumContentsLength(handle: QComboBoxH; characters: Integer); cdecl; external QtIntf name 'QComboBox_setMinimumContentsLength';
procedure QComboBox_iconSize(handle: QComboBoxH; retval: PSize); cdecl; external QtIntf name 'QComboBox_iconSize';
procedure QComboBox_setIconSize(handle: QComboBoxH; size: PSize); cdecl; external QtIntf name 'QComboBox_setIconSize';
function QComboBox_isEditable(handle: QComboBoxH): Boolean; cdecl; external QtIntf name 'QComboBox_isEditable';
procedure QComboBox_setEditable(handle: QComboBoxH; editable: Boolean); cdecl; external QtIntf name 'QComboBox_setEditable';
procedure QComboBox_setLineEdit(handle: QComboBoxH; edit: QLineEditH); cdecl; external QtIntf name 'QComboBox_setLineEdit';
function QComboBox_lineEdit(handle: QComboBoxH): QLineEditH; cdecl; external QtIntf name 'QComboBox_lineEdit';
procedure QComboBox_setValidator(handle: QComboBoxH; v: QValidatorH); cdecl; external QtIntf name 'QComboBox_setValidator';
function QComboBox_validator(handle: QComboBoxH): QValidatorH; cdecl; external QtIntf name 'QComboBox_validator';
procedure QComboBox_setCompleter(handle: QComboBoxH; c: QCompleterH); cdecl; external QtIntf name 'QComboBox_setCompleter';
function QComboBox_completer(handle: QComboBoxH): QCompleterH; cdecl; external QtIntf name 'QComboBox_completer';
function QComboBox_itemDelegate(handle: QComboBoxH): QAbstractItemDelegateH; cdecl; external QtIntf name 'QComboBox_itemDelegate';
procedure QComboBox_setItemDelegate(handle: QComboBoxH; delegate: QAbstractItemDelegateH); cdecl; external QtIntf name 'QComboBox_setItemDelegate';
function QComboBox_model(handle: QComboBoxH): QAbstractItemModelH; cdecl; external QtIntf name 'QComboBox_model';
procedure QComboBox_setModel(handle: QComboBoxH; model: QAbstractItemModelH); cdecl; external QtIntf name 'QComboBox_setModel';
procedure QComboBox_rootModelIndex(handle: QComboBoxH; retval: QModelIndexH); cdecl; external QtIntf name 'QComboBox_rootModelIndex';
procedure QComboBox_setRootModelIndex(handle: QComboBoxH; index: QModelIndexH); cdecl; external QtIntf name 'QComboBox_setRootModelIndex';
function QComboBox_modelColumn(handle: QComboBoxH): Integer; cdecl; external QtIntf name 'QComboBox_modelColumn';
procedure QComboBox_setModelColumn(handle: QComboBoxH; visibleColumn: Integer); cdecl; external QtIntf name 'QComboBox_setModelColumn';
function QComboBox_currentIndex(handle: QComboBoxH): Integer; cdecl; external QtIntf name 'QComboBox_currentIndex';
procedure QComboBox_currentText(handle: QComboBoxH; retval: PWideString); cdecl; external QtIntf name 'QComboBox_currentText';
procedure QComboBox_itemText(handle: QComboBoxH; retval: PWideString; index: Integer); cdecl; external QtIntf name 'QComboBox_itemText';
procedure QComboBox_itemIcon(handle: QComboBoxH; retval: QIconH; index: Integer); cdecl; external QtIntf name 'QComboBox_itemIcon';
procedure QComboBox_itemData(handle: QComboBoxH; retval: QVariantH; index: Integer; role: QtItemDataRole = QtUserRole); cdecl; external QtIntf name 'QComboBox_itemData';
procedure QComboBox_addItem(handle: QComboBoxH; text: PWideString; userData: QVariantH = nil); overload; cdecl; external QtIntf name 'QComboBox_addItem';
procedure QComboBox_addItem(handle: QComboBoxH; icon: QIconH; text: PWideString; userData: QVariantH = nil); overload; cdecl; external QtIntf name 'QComboBox_addItem2';
procedure QComboBox_addItems(handle: QComboBoxH; texts: QStringListH); cdecl; external QtIntf name 'QComboBox_addItems';
procedure QComboBox_insertItem(handle: QComboBoxH; index: Integer; text: PWideString; userData: QVariantH = nil); overload; cdecl; external QtIntf name 'QComboBox_insertItem';
procedure QComboBox_insertItem(handle: QComboBoxH; index: Integer; icon: QIconH; text: PWideString; userData: QVariantH = nil); overload; cdecl; external QtIntf name 'QComboBox_insertItem2';
procedure QComboBox_insertItems(handle: QComboBoxH; index: Integer; texts: QStringListH); cdecl; external QtIntf name 'QComboBox_insertItems';
procedure QComboBox_removeItem(handle: QComboBoxH; index: Integer); cdecl; external QtIntf name 'QComboBox_removeItem';
procedure QComboBox_setItemText(handle: QComboBoxH; index: Integer; text: PWideString); cdecl; external QtIntf name 'QComboBox_setItemText';
procedure QComboBox_setItemIcon(handle: QComboBoxH; index: Integer; icon: QIconH); cdecl; external QtIntf name 'QComboBox_setItemIcon';
procedure QComboBox_setItemData(handle: QComboBoxH; index: Integer; value: QVariantH; role: QtItemDataRole = QtUserRole); cdecl; external QtIntf name 'QComboBox_setItemData';
function QComboBox_view(handle: QComboBoxH): QAbstractItemViewH; cdecl; external QtIntf name 'QComboBox_view';
procedure QComboBox_setView(handle: QComboBoxH; itemView: QAbstractItemViewH); cdecl; external QtIntf name 'QComboBox_setView';
procedure QComboBox_sizeHint(handle: QComboBoxH; retval: PSize); cdecl; external QtIntf name 'QComboBox_sizeHint';
procedure QComboBox_minimumSizeHint(handle: QComboBoxH; retval: PSize); cdecl; external QtIntf name 'QComboBox_minimumSizeHint';
procedure QComboBox_showPopup(handle: QComboBoxH); cdecl; external QtIntf name 'QComboBox_showPopup';
procedure QComboBox_hidePopup(handle: QComboBoxH); cdecl; external QtIntf name 'QComboBox_hidePopup';
function QComboBox_event(handle: QComboBoxH; event: QEventH): Boolean; cdecl; external QtIntf name 'QComboBox_event';
procedure QComboBox_clear(handle: QComboBoxH); cdecl; external QtIntf name 'QComboBox_clear';
procedure QComboBox_clearEditText(handle: QComboBoxH); cdecl; external QtIntf name 'QComboBox_clearEditText';
procedure QComboBox_setEditText(handle: QComboBoxH; text: PWideString); cdecl; external QtIntf name 'QComboBox_setEditText';
procedure QComboBox_setCurrentIndex(handle: QComboBoxH; index: Integer); cdecl; external QtIntf name 'QComboBox_setCurrentIndex';


type
  QComboBox_editTextChanged_Event = procedure (p1: PWideString) of object cdecl;
  QComboBox_activated_Event = procedure (index: Integer) of object cdecl;
  QComboBox_activated2_Event = procedure (p1: PWideString) of object cdecl;
  QComboBox_highlighted_Event = procedure (index: Integer) of object cdecl;
  QComboBox_highlighted2_Event = procedure (p1: PWideString) of object cdecl;
  QComboBox_currentIndexChanged_Event = procedure (index: Integer) of object cdecl;
  QComboBox_currentIndexChanged2_Event = procedure (p1: PWideString) of object cdecl;


function QCheckBox_create(parent: QWidgetH = nil): QCheckBoxH; overload; cdecl; external QtIntf name 'QCheckBox_create';
procedure QCheckBox_destroy(handle: QCheckBoxH); cdecl; external QtIntf name 'QCheckBox_destroy'; 
function QCheckBox_create(text: PWideString; parent: QWidgetH = nil): QCheckBoxH; overload; cdecl; external QtIntf name 'QCheckBox_create2';
procedure QCheckBox_sizeHint(handle: QCheckBoxH; retval: PSize); cdecl; external QtIntf name 'QCheckBox_sizeHint';
procedure QCheckBox_setTristate(handle: QCheckBoxH; y: Boolean = True); cdecl; external QtIntf name 'QCheckBox_setTristate';
function QCheckBox_isTristate(handle: QCheckBoxH): Boolean; cdecl; external QtIntf name 'QCheckBox_isTristate';
function QCheckBox_checkState(handle: QCheckBoxH): QtCheckState; cdecl; external QtIntf name 'QCheckBox_checkState';
procedure QCheckBox_setCheckState(handle: QCheckBoxH; state: QtCheckState); cdecl; external QtIntf name 'QCheckBox_setCheckState';


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


function QSlider_create(parent: QWidgetH = nil): QSliderH; overload; cdecl; external QtIntf name 'QSlider_create';
procedure QSlider_destroy(handle: QSliderH); cdecl; external QtIntf name 'QSlider_destroy'; 
function QSlider_create(orientation: QtOrientation; parent: QWidgetH = nil): QSliderH; overload; cdecl; external QtIntf name 'QSlider_create2';
procedure QSlider_sizeHint(handle: QSliderH; retval: PSize); cdecl; external QtIntf name 'QSlider_sizeHint';
procedure QSlider_minimumSizeHint(handle: QSliderH; retval: PSize); cdecl; external QtIntf name 'QSlider_minimumSizeHint';
procedure QSlider_setTickPosition(handle: QSliderH; position: QSliderTickPosition); cdecl; external QtIntf name 'QSlider_setTickPosition';
function QSlider_tickPosition(handle: QSliderH): QSliderTickPosition; cdecl; external QtIntf name 'QSlider_tickPosition';
procedure QSlider_setTickInterval(handle: QSliderH; ti: Integer); cdecl; external QtIntf name 'QSlider_setTickInterval';
function QSlider_tickInterval(handle: QSliderH): Integer; cdecl; external QtIntf name 'QSlider_tickInterval';
function QSlider_event(handle: QSliderH; event: QEventH): Boolean; cdecl; external QtIntf name 'QSlider_event';

function QTextBrowser_create(parent: QWidgetH = nil): QTextBrowserH; cdecl; external QtIntf name 'QTextBrowser_create';
procedure QTextBrowser_destroy(handle: QTextBrowserH); cdecl; external QtIntf name 'QTextBrowser_destroy'; 
procedure QTextBrowser_source(handle: QTextBrowserH; retval: QUrlH); cdecl; external QtIntf name 'QTextBrowser_source';
procedure QTextBrowser_searchPaths(handle: QTextBrowserH; retval: QStringListH); cdecl; external QtIntf name 'QTextBrowser_searchPaths';
procedure QTextBrowser_setSearchPaths(handle: QTextBrowserH; paths: QStringListH); cdecl; external QtIntf name 'QTextBrowser_setSearchPaths';
procedure QTextBrowser_loadResource(handle: QTextBrowserH; retval: QVariantH; _type: Integer; name: QUrlH); cdecl; external QtIntf name 'QTextBrowser_loadResource';
function QTextBrowser_isBackwardAvailable(handle: QTextBrowserH): Boolean; cdecl; external QtIntf name 'QTextBrowser_isBackwardAvailable';
function QTextBrowser_isForwardAvailable(handle: QTextBrowserH): Boolean; cdecl; external QtIntf name 'QTextBrowser_isForwardAvailable';
procedure QTextBrowser_clearHistory(handle: QTextBrowserH); cdecl; external QtIntf name 'QTextBrowser_clearHistory';
function QTextBrowser_openExternalLinks(handle: QTextBrowserH): Boolean; cdecl; external QtIntf name 'QTextBrowser_openExternalLinks';
procedure QTextBrowser_setOpenExternalLinks(handle: QTextBrowserH; open: Boolean); cdecl; external QtIntf name 'QTextBrowser_setOpenExternalLinks';
procedure QTextBrowser_setSource(handle: QTextBrowserH; name: QUrlH); cdecl; external QtIntf name 'QTextBrowser_setSource';
procedure QTextBrowser_backward(handle: QTextBrowserH); cdecl; external QtIntf name 'QTextBrowser_backward';
procedure QTextBrowser_forward(handle: QTextBrowserH); cdecl; external QtIntf name 'QTextBrowser_forward';
procedure QTextBrowser_home(handle: QTextBrowserH); cdecl; external QtIntf name 'QTextBrowser_home';
procedure QTextBrowser_reload(handle: QTextBrowserH); cdecl; external QtIntf name 'QTextBrowser_reload';


type
  QTextBrowser_backwardAvailable_Event = procedure (p1: Boolean) of object cdecl;
  QTextBrowser_forwardAvailable_Event = procedure (p1: Boolean) of object cdecl;
  QTextBrowser_sourceChanged_Event = procedure (p1: QUrlH) of object cdecl;
  QTextBrowser_highlighted_Event = procedure (p1: QUrlH) of object cdecl;
  QTextBrowser_highlighted2_Event = procedure (p1: PWideString) of object cdecl;
  QTextBrowser_anchorClicked_Event = procedure (p1: QUrlH) of object cdecl;


function QLabel_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QLabelH; overload; cdecl; external QtIntf name 'QLabel_create';
procedure QLabel_destroy(handle: QLabelH); cdecl; external QtIntf name 'QLabel_destroy'; 
function QLabel_create(text: PWideString; parent: QWidgetH = nil; f: QtWindowFlags = 0): QLabelH; overload; cdecl; external QtIntf name 'QLabel_create2';
procedure QLabel_text(handle: QLabelH; retval: PWideString); cdecl; external QtIntf name 'QLabel_text';
function QLabel_pixmap(handle: QLabelH): QPixmapH; cdecl; external QtIntf name 'QLabel_pixmap';
function QLabel_picture(handle: QLabelH): QPictureH; cdecl; external QtIntf name 'QLabel_picture';
function QLabel_movie(handle: QLabelH): QMovieH; cdecl; external QtIntf name 'QLabel_movie';
function QLabel_textFormat(handle: QLabelH): QtTextFormat; cdecl; external QtIntf name 'QLabel_textFormat';
procedure QLabel_setTextFormat(handle: QLabelH; p1: QtTextFormat); cdecl; external QtIntf name 'QLabel_setTextFormat';
function QLabel_alignment(handle: QLabelH): QtAlignment; cdecl; external QtIntf name 'QLabel_alignment';
procedure QLabel_setAlignment(handle: QLabelH; p1: QtAlignment); cdecl; external QtIntf name 'QLabel_setAlignment';
procedure QLabel_setWordWrap(handle: QLabelH; _on: Boolean); cdecl; external QtIntf name 'QLabel_setWordWrap';
function QLabel_wordWrap(handle: QLabelH): Boolean; cdecl; external QtIntf name 'QLabel_wordWrap';
function QLabel_indent(handle: QLabelH): Integer; cdecl; external QtIntf name 'QLabel_indent';
procedure QLabel_setIndent(handle: QLabelH; p1: Integer); cdecl; external QtIntf name 'QLabel_setIndent';
function QLabel_margin(handle: QLabelH): Integer; cdecl; external QtIntf name 'QLabel_margin';
procedure QLabel_setMargin(handle: QLabelH; p1: Integer); cdecl; external QtIntf name 'QLabel_setMargin';
function QLabel_hasScaledContents(handle: QLabelH): Boolean; cdecl; external QtIntf name 'QLabel_hasScaledContents';
procedure QLabel_setScaledContents(handle: QLabelH; p1: Boolean); cdecl; external QtIntf name 'QLabel_setScaledContents';
procedure QLabel_sizeHint(handle: QLabelH; retval: PSize); cdecl; external QtIntf name 'QLabel_sizeHint';
procedure QLabel_minimumSizeHint(handle: QLabelH; retval: PSize); cdecl; external QtIntf name 'QLabel_minimumSizeHint';
procedure QLabel_setBuddy(handle: QLabelH; p1: QWidgetH); cdecl; external QtIntf name 'QLabel_setBuddy';
function QLabel_buddy(handle: QLabelH): QWidgetH; cdecl; external QtIntf name 'QLabel_buddy';
function QLabel_heightForWidth(handle: QLabelH; p1: Integer): Integer; cdecl; external QtIntf name 'QLabel_heightForWidth';
function QLabel_openExternalLinks(handle: QLabelH): Boolean; cdecl; external QtIntf name 'QLabel_openExternalLinks';
procedure QLabel_setOpenExternalLinks(handle: QLabelH; open: Boolean); cdecl; external QtIntf name 'QLabel_setOpenExternalLinks';
procedure QLabel_setTextInteractionFlags(handle: QLabelH; flags: QtTextInteractionFlags); cdecl; external QtIntf name 'QLabel_setTextInteractionFlags';
function QLabel_textInteractionFlags(handle: QLabelH): QtTextInteractionFlags; cdecl; external QtIntf name 'QLabel_textInteractionFlags';
procedure QLabel_setText(handle: QLabelH; p1: PWideString); cdecl; external QtIntf name 'QLabel_setText';
procedure QLabel_setPixmap(handle: QLabelH; p1: QPixmapH); cdecl; external QtIntf name 'QLabel_setPixmap';
procedure QLabel_setPicture(handle: QLabelH; p1: QPictureH); cdecl; external QtIntf name 'QLabel_setPicture';
procedure QLabel_setMovie(handle: QLabelH; movie: QMovieH); cdecl; external QtIntf name 'QLabel_setMovie';
procedure QLabel_setNum(handle: QLabelH; p1: Integer); overload; cdecl; external QtIntf name 'QLabel_setNum';
procedure QLabel_setNum(handle: QLabelH; p1: Double); overload; cdecl; external QtIntf name 'QLabel_setNum2';
procedure QLabel_clear(handle: QLabelH); cdecl; external QtIntf name 'QLabel_clear';


type
  QLabel_linkActivated_Event = procedure (link: PWideString) of object cdecl;
  QLabel_linkHovered_Event = procedure (link: PWideString) of object cdecl;


function QGroupBox_create(parent: QWidgetH = nil): QGroupBoxH; overload; cdecl; external QtIntf name 'QGroupBox_create';
procedure QGroupBox_destroy(handle: QGroupBoxH); cdecl; external QtIntf name 'QGroupBox_destroy'; 
function QGroupBox_create(title: PWideString; parent: QWidgetH = nil): QGroupBoxH; overload; cdecl; external QtIntf name 'QGroupBox_create2';
procedure QGroupBox_title(handle: QGroupBoxH; retval: PWideString); cdecl; external QtIntf name 'QGroupBox_title';
procedure QGroupBox_setTitle(handle: QGroupBoxH; title: PWideString); cdecl; external QtIntf name 'QGroupBox_setTitle';
function QGroupBox_alignment(handle: QGroupBoxH): QtAlignment; cdecl; external QtIntf name 'QGroupBox_alignment';
procedure QGroupBox_setAlignment(handle: QGroupBoxH; alignment: Integer); cdecl; external QtIntf name 'QGroupBox_setAlignment';
procedure QGroupBox_minimumSizeHint(handle: QGroupBoxH; retval: PSize); cdecl; external QtIntf name 'QGroupBox_minimumSizeHint';
function QGroupBox_isFlat(handle: QGroupBoxH): Boolean; cdecl; external QtIntf name 'QGroupBox_isFlat';
procedure QGroupBox_setFlat(handle: QGroupBoxH; flat: Boolean); cdecl; external QtIntf name 'QGroupBox_setFlat';
function QGroupBox_isCheckable(handle: QGroupBoxH): Boolean; cdecl; external QtIntf name 'QGroupBox_isCheckable';
procedure QGroupBox_setCheckable(handle: QGroupBoxH; checkable: Boolean); cdecl; external QtIntf name 'QGroupBox_setCheckable';
function QGroupBox_isChecked(handle: QGroupBoxH): Boolean; cdecl; external QtIntf name 'QGroupBox_isChecked';
procedure QGroupBox_setChecked(handle: QGroupBoxH; checked: Boolean); cdecl; external QtIntf name 'QGroupBox_setChecked';


type
  QGroupBox_clicked_Event = procedure (checked: Boolean = False) of object cdecl;
  QGroupBox_clicked2_Event = procedure () of object cdecl;
  QGroupBox_toggled_Event = procedure (p1: Boolean) of object cdecl;



type
  QTabWidgetTabPosition = ( // QTabWidget::TabPosition (1)
    QTabWidgetNorth, QTabWidgetSouth, QTabWidgetWest, QTabWidgetEast );

  QTabWidgetTabShape = ( // QTabWidget::TabShape (1)
    QTabWidgetRounded, QTabWidgetTriangular );

function QTabWidget_create(parent: QWidgetH = nil): QTabWidgetH; cdecl; external QtIntf name 'QTabWidget_create';
procedure QTabWidget_destroy(handle: QTabWidgetH); cdecl; external QtIntf name 'QTabWidget_destroy'; 
function QTabWidget_addTab(handle: QTabWidgetH; widget: QWidgetH; p2: PWideString): Integer; overload; cdecl; external QtIntf name 'QTabWidget_addTab';
function QTabWidget_addTab(handle: QTabWidgetH; widget: QWidgetH; icon: QIconH; _label: PWideString): Integer; overload; cdecl; external QtIntf name 'QTabWidget_addTab2';
function QTabWidget_insertTab(handle: QTabWidgetH; index: Integer; widget: QWidgetH; p3: PWideString): Integer; overload; cdecl; external QtIntf name 'QTabWidget_insertTab';
function QTabWidget_insertTab(handle: QTabWidgetH; index: Integer; widget: QWidgetH; icon: QIconH; _label: PWideString): Integer; overload; cdecl; external QtIntf name 'QTabWidget_insertTab2';
procedure QTabWidget_removeTab(handle: QTabWidgetH; index: Integer); cdecl; external QtIntf name 'QTabWidget_removeTab';
function QTabWidget_isTabEnabled(handle: QTabWidgetH; index: Integer): Boolean; cdecl; external QtIntf name 'QTabWidget_isTabEnabled';
procedure QTabWidget_setTabEnabled(handle: QTabWidgetH; index: Integer; p2: Boolean); cdecl; external QtIntf name 'QTabWidget_setTabEnabled';
procedure QTabWidget_tabText(handle: QTabWidgetH; retval: PWideString; index: Integer); cdecl; external QtIntf name 'QTabWidget_tabText';
procedure QTabWidget_setTabText(handle: QTabWidgetH; index: Integer; p2: PWideString); cdecl; external QtIntf name 'QTabWidget_setTabText';
procedure QTabWidget_tabIcon(handle: QTabWidgetH; retval: QIconH; index: Integer); cdecl; external QtIntf name 'QTabWidget_tabIcon';
procedure QTabWidget_setTabIcon(handle: QTabWidgetH; index: Integer; icon: QIconH); cdecl; external QtIntf name 'QTabWidget_setTabIcon';
procedure QTabWidget_setTabToolTip(handle: QTabWidgetH; index: Integer; tip: PWideString); cdecl; external QtIntf name 'QTabWidget_setTabToolTip';
procedure QTabWidget_tabToolTip(handle: QTabWidgetH; retval: PWideString; index: Integer); cdecl; external QtIntf name 'QTabWidget_tabToolTip';
procedure QTabWidget_setTabWhatsThis(handle: QTabWidgetH; index: Integer; text: PWideString); cdecl; external QtIntf name 'QTabWidget_setTabWhatsThis';
procedure QTabWidget_tabWhatsThis(handle: QTabWidgetH; retval: PWideString; index: Integer); cdecl; external QtIntf name 'QTabWidget_tabWhatsThis';
function QTabWidget_currentIndex(handle: QTabWidgetH): Integer; cdecl; external QtIntf name 'QTabWidget_currentIndex';
function QTabWidget_currentWidget(handle: QTabWidgetH): QWidgetH; cdecl; external QtIntf name 'QTabWidget_currentWidget';
function QTabWidget_widget(handle: QTabWidgetH; index: Integer): QWidgetH; cdecl; external QtIntf name 'QTabWidget_widget';
function QTabWidget_indexOf(handle: QTabWidgetH; widget: QWidgetH): Integer; cdecl; external QtIntf name 'QTabWidget_indexOf';
function QTabWidget_count(handle: QTabWidgetH): Integer; cdecl; external QtIntf name 'QTabWidget_count';
function QTabWidget_tabPosition(handle: QTabWidgetH): QTabWidgetTabPosition; cdecl; external QtIntf name 'QTabWidget_tabPosition';
procedure QTabWidget_setTabPosition(handle: QTabWidgetH; p1: QTabWidgetTabPosition); cdecl; external QtIntf name 'QTabWidget_setTabPosition';
function QTabWidget_tabShape(handle: QTabWidgetH): QTabWidgetTabShape; cdecl; external QtIntf name 'QTabWidget_tabShape';
procedure QTabWidget_setTabShape(handle: QTabWidgetH; s: QTabWidgetTabShape); cdecl; external QtIntf name 'QTabWidget_setTabShape';
procedure QTabWidget_sizeHint(handle: QTabWidgetH; retval: PSize); cdecl; external QtIntf name 'QTabWidget_sizeHint';
procedure QTabWidget_minimumSizeHint(handle: QTabWidgetH; retval: PSize); cdecl; external QtIntf name 'QTabWidget_minimumSizeHint';
procedure QTabWidget_setCornerWidget(handle: QTabWidgetH; w: QWidgetH; corner: QtCorner = QtTopRightCorner); cdecl; external QtIntf name 'QTabWidget_setCornerWidget';
function QTabWidget_cornerWidget(handle: QTabWidgetH; corner: QtCorner = QtTopRightCorner): QWidgetH; cdecl; external QtIntf name 'QTabWidget_cornerWidget';
function QTabWidget_elideMode(handle: QTabWidgetH): QtTextElideMode; cdecl; external QtIntf name 'QTabWidget_elideMode';
procedure QTabWidget_setElideMode(handle: QTabWidgetH; p1: QtTextElideMode); cdecl; external QtIntf name 'QTabWidget_setElideMode';
procedure QTabWidget_iconSize(handle: QTabWidgetH; retval: PSize); cdecl; external QtIntf name 'QTabWidget_iconSize';
procedure QTabWidget_setIconSize(handle: QTabWidgetH; size: PSize); cdecl; external QtIntf name 'QTabWidget_setIconSize';
function QTabWidget_usesScrollButtons(handle: QTabWidgetH): Boolean; cdecl; external QtIntf name 'QTabWidget_usesScrollButtons';
procedure QTabWidget_setUsesScrollButtons(handle: QTabWidgetH; useButtons: Boolean); cdecl; external QtIntf name 'QTabWidget_setUsesScrollButtons';
procedure QTabWidget_setCurrentIndex(handle: QTabWidgetH; index: Integer); cdecl; external QtIntf name 'QTabWidget_setCurrentIndex';
procedure QTabWidget_setCurrentWidget(handle: QTabWidgetH; widget: QWidgetH); cdecl; external QtIntf name 'QTabWidget_setCurrentWidget';


type
  QTabWidget_currentChanged_Event = procedure (index: Integer) of object cdecl;


function QLCLTabWidget_tabBarHandle(protectedhandle: QTabWidgetH): QTabBarH; cdecl; external QtIntf name 'QLCLTabWidget_tabBarHandle';


type
  QTabBarShape = ( // QTabBar::Shape (1)
    QTabBarRoundedNorth, QTabBarRoundedSouth, QTabBarRoundedWest, QTabBarRoundedEast, QTabBarTriangularNorth, QTabBarTriangularSouth, QTabBarTriangularWest, QTabBarTriangularEast );

function QTabBar_create(parent: QWidgetH = nil): QTabBarH; cdecl; external QtIntf name 'QTabBar_create';
procedure QTabBar_destroy(handle: QTabBarH); cdecl; external QtIntf name 'QTabBar_destroy'; 
function QTabBar_shape(handle: QTabBarH): QTabBarShape; cdecl; external QtIntf name 'QTabBar_shape';
procedure QTabBar_setShape(handle: QTabBarH; shape: QTabBarShape); cdecl; external QtIntf name 'QTabBar_setShape';
function QTabBar_addTab(handle: QTabBarH; text: PWideString): Integer; overload; cdecl; external QtIntf name 'QTabBar_addTab';
function QTabBar_addTab(handle: QTabBarH; icon: QIconH; text: PWideString): Integer; overload; cdecl; external QtIntf name 'QTabBar_addTab2';
function QTabBar_insertTab(handle: QTabBarH; index: Integer; text: PWideString): Integer; overload; cdecl; external QtIntf name 'QTabBar_insertTab';
function QTabBar_insertTab(handle: QTabBarH; index: Integer; icon: QIconH; text: PWideString): Integer; overload; cdecl; external QtIntf name 'QTabBar_insertTab2';
procedure QTabBar_removeTab(handle: QTabBarH; index: Integer); cdecl; external QtIntf name 'QTabBar_removeTab';
function QTabBar_isTabEnabled(handle: QTabBarH; index: Integer): Boolean; cdecl; external QtIntf name 'QTabBar_isTabEnabled';
procedure QTabBar_setTabEnabled(handle: QTabBarH; index: Integer; p2: Boolean); cdecl; external QtIntf name 'QTabBar_setTabEnabled';
procedure QTabBar_tabText(handle: QTabBarH; retval: PWideString; index: Integer); cdecl; external QtIntf name 'QTabBar_tabText';
procedure QTabBar_setTabText(handle: QTabBarH; index: Integer; text: PWideString); cdecl; external QtIntf name 'QTabBar_setTabText';
procedure QTabBar_tabTextColor(handle: QTabBarH; retval: PQColor; index: Integer); cdecl; external QtIntf name 'QTabBar_tabTextColor';
procedure QTabBar_setTabTextColor(handle: QTabBarH; index: Integer; color: PQColor); cdecl; external QtIntf name 'QTabBar_setTabTextColor';
procedure QTabBar_tabIcon(handle: QTabBarH; retval: QIconH; index: Integer); cdecl; external QtIntf name 'QTabBar_tabIcon';
procedure QTabBar_setTabIcon(handle: QTabBarH; index: Integer; icon: QIconH); cdecl; external QtIntf name 'QTabBar_setTabIcon';
function QTabBar_elideMode(handle: QTabBarH): QtTextElideMode; cdecl; external QtIntf name 'QTabBar_elideMode';
procedure QTabBar_setElideMode(handle: QTabBarH; p1: QtTextElideMode); cdecl; external QtIntf name 'QTabBar_setElideMode';
procedure QTabBar_setTabToolTip(handle: QTabBarH; index: Integer; tip: PWideString); cdecl; external QtIntf name 'QTabBar_setTabToolTip';
procedure QTabBar_tabToolTip(handle: QTabBarH; retval: PWideString; index: Integer); cdecl; external QtIntf name 'QTabBar_tabToolTip';
procedure QTabBar_setTabWhatsThis(handle: QTabBarH; index: Integer; text: PWideString); cdecl; external QtIntf name 'QTabBar_setTabWhatsThis';
procedure QTabBar_tabWhatsThis(handle: QTabBarH; retval: PWideString; index: Integer); cdecl; external QtIntf name 'QTabBar_tabWhatsThis';
procedure QTabBar_setTabData(handle: QTabBarH; index: Integer; data: QVariantH); cdecl; external QtIntf name 'QTabBar_setTabData';
procedure QTabBar_tabData(handle: QTabBarH; retval: QVariantH; index: Integer); cdecl; external QtIntf name 'QTabBar_tabData';
procedure QTabBar_tabRect(handle: QTabBarH; retval: PRect; index: Integer); cdecl; external QtIntf name 'QTabBar_tabRect';
function QTabBar_currentIndex(handle: QTabBarH): Integer; cdecl; external QtIntf name 'QTabBar_currentIndex';
function QTabBar_count(handle: QTabBarH): Integer; cdecl; external QtIntf name 'QTabBar_count';
procedure QTabBar_sizeHint(handle: QTabBarH; retval: PSize); cdecl; external QtIntf name 'QTabBar_sizeHint';
procedure QTabBar_minimumSizeHint(handle: QTabBarH; retval: PSize); cdecl; external QtIntf name 'QTabBar_minimumSizeHint';
procedure QTabBar_setDrawBase(handle: QTabBarH; drawTheBase: Boolean); cdecl; external QtIntf name 'QTabBar_setDrawBase';
function QTabBar_drawBase(handle: QTabBarH): Boolean; cdecl; external QtIntf name 'QTabBar_drawBase';
procedure QTabBar_iconSize(handle: QTabBarH; retval: PSize); cdecl; external QtIntf name 'QTabBar_iconSize';
procedure QTabBar_setIconSize(handle: QTabBarH; size: PSize); cdecl; external QtIntf name 'QTabBar_setIconSize';
function QTabBar_usesScrollButtons(handle: QTabBarH): Boolean; cdecl; external QtIntf name 'QTabBar_usesScrollButtons';
procedure QTabBar_setUsesScrollButtons(handle: QTabBarH; useButtons: Boolean); cdecl; external QtIntf name 'QTabBar_setUsesScrollButtons';
procedure QTabBar_setCurrentIndex(handle: QTabBarH; index: Integer); cdecl; external QtIntf name 'QTabBar_setCurrentIndex';


type
  QTabBar_currentChanged_Event = procedure (index: Integer) of object cdecl;



type
  QProgressBarDirection = ( // QProgressBar::Direction (1)
    QProgressBarTopToBottom, QProgressBarBottomToTop );

function QProgressBar_create(parent: QWidgetH = nil): QProgressBarH; cdecl; external QtIntf name 'QProgressBar_create';
procedure QProgressBar_destroy(handle: QProgressBarH); cdecl; external QtIntf name 'QProgressBar_destroy'; 
function QProgressBar_minimum(handle: QProgressBarH): Integer; cdecl; external QtIntf name 'QProgressBar_minimum';
function QProgressBar_maximum(handle: QProgressBarH): Integer; cdecl; external QtIntf name 'QProgressBar_maximum';
procedure QProgressBar_setRange(handle: QProgressBarH; minimum: Integer; maximum: Integer); cdecl; external QtIntf name 'QProgressBar_setRange';
function QProgressBar_value(handle: QProgressBarH): Integer; cdecl; external QtIntf name 'QProgressBar_value';
procedure QProgressBar_text(handle: QProgressBarH; retval: PWideString); cdecl; external QtIntf name 'QProgressBar_text';
procedure QProgressBar_setTextVisible(handle: QProgressBarH; visible: Boolean); cdecl; external QtIntf name 'QProgressBar_setTextVisible';
function QProgressBar_isTextVisible(handle: QProgressBarH): Boolean; cdecl; external QtIntf name 'QProgressBar_isTextVisible';
function QProgressBar_alignment(handle: QProgressBarH): QtAlignment; cdecl; external QtIntf name 'QProgressBar_alignment';
procedure QProgressBar_setAlignment(handle: QProgressBarH; alignment: QtAlignment); cdecl; external QtIntf name 'QProgressBar_setAlignment';
procedure QProgressBar_sizeHint(handle: QProgressBarH; retval: PSize); cdecl; external QtIntf name 'QProgressBar_sizeHint';
procedure QProgressBar_minimumSizeHint(handle: QProgressBarH; retval: PSize); cdecl; external QtIntf name 'QProgressBar_minimumSizeHint';
function QProgressBar_orientation(handle: QProgressBarH): QtOrientation; cdecl; external QtIntf name 'QProgressBar_orientation';
procedure QProgressBar_setInvertedAppearance(handle: QProgressBarH; invert: Boolean); cdecl; external QtIntf name 'QProgressBar_setInvertedAppearance';
function QProgressBar_invertedAppearance(handle: QProgressBarH): Boolean; cdecl; external QtIntf name 'QProgressBar_invertedAppearance';
procedure QProgressBar_setTextDirection(handle: QProgressBarH; textDirection: QProgressBarDirection); cdecl; external QtIntf name 'QProgressBar_setTextDirection';
function QProgressBar_textDirection(handle: QProgressBarH): QProgressBarDirection; cdecl; external QtIntf name 'QProgressBar_textDirection';
procedure QProgressBar_setFormat(handle: QProgressBarH; format: PWideString); cdecl; external QtIntf name 'QProgressBar_setFormat';
procedure QProgressBar_format(handle: QProgressBarH; retval: PWideString); cdecl; external QtIntf name 'QProgressBar_format';
procedure QProgressBar_reset(handle: QProgressBarH); cdecl; external QtIntf name 'QProgressBar_reset';
procedure QProgressBar_setMinimum(handle: QProgressBarH; minimum: Integer); cdecl; external QtIntf name 'QProgressBar_setMinimum';
procedure QProgressBar_setMaximum(handle: QProgressBarH; maximum: Integer); cdecl; external QtIntf name 'QProgressBar_setMaximum';
procedure QProgressBar_setValue(handle: QProgressBarH; value: Integer); cdecl; external QtIntf name 'QProgressBar_setValue';
procedure QProgressBar_setOrientation(handle: QProgressBarH; p1: QtOrientation); cdecl; external QtIntf name 'QProgressBar_setOrientation';


type
  QProgressBar_valueChanged_Event = procedure (value: Integer) of object cdecl;


function QStatusBar_create(parent: QWidgetH = nil): QStatusBarH; cdecl; external QtIntf name 'QStatusBar_create';
procedure QStatusBar_destroy(handle: QStatusBarH); cdecl; external QtIntf name 'QStatusBar_destroy'; 
procedure QStatusBar_addWidget(handle: QStatusBarH; widget: QWidgetH; stretch: Integer = 0); cdecl; external QtIntf name 'QStatusBar_addWidget';
function QStatusBar_insertWidget(handle: QStatusBarH; index: Integer; widget: QWidgetH; stretch: Integer = 0): Integer; cdecl; external QtIntf name 'QStatusBar_insertWidget';
procedure QStatusBar_addPermanentWidget(handle: QStatusBarH; widget: QWidgetH; stretch: Integer = 0); cdecl; external QtIntf name 'QStatusBar_addPermanentWidget';
function QStatusBar_insertPermanentWidget(handle: QStatusBarH; index: Integer; widget: QWidgetH; stretch: Integer = 0): Integer; cdecl; external QtIntf name 'QStatusBar_insertPermanentWidget';
procedure QStatusBar_removeWidget(handle: QStatusBarH; widget: QWidgetH); cdecl; external QtIntf name 'QStatusBar_removeWidget';
procedure QStatusBar_setSizeGripEnabled(handle: QStatusBarH; p1: Boolean); cdecl; external QtIntf name 'QStatusBar_setSizeGripEnabled';
function QStatusBar_isSizeGripEnabled(handle: QStatusBarH): Boolean; cdecl; external QtIntf name 'QStatusBar_isSizeGripEnabled';
procedure QStatusBar_currentMessage(handle: QStatusBarH; retval: PWideString); cdecl; external QtIntf name 'QStatusBar_currentMessage';
procedure QStatusBar_showMessage(handle: QStatusBarH; text: PWideString; timeout: Integer = 0); cdecl; external QtIntf name 'QStatusBar_showMessage';
procedure QStatusBar_clearMessage(handle: QStatusBarH); cdecl; external QtIntf name 'QStatusBar_clearMessage';


type
  QStatusBar_messageChanged_Event = procedure (text: PWideString) of object cdecl;


function QToolBox_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QToolBoxH; cdecl; external QtIntf name 'QToolBox_create';
procedure QToolBox_destroy(handle: QToolBoxH); cdecl; external QtIntf name 'QToolBox_destroy'; 
function QToolBox_addItem(handle: QToolBoxH; widget: QWidgetH; text: PWideString): Integer; overload; cdecl; external QtIntf name 'QToolBox_addItem';
function QToolBox_addItem(handle: QToolBoxH; widget: QWidgetH; icon: QIconH; text: PWideString): Integer; overload; cdecl; external QtIntf name 'QToolBox_addItem2';
function QToolBox_insertItem(handle: QToolBoxH; index: Integer; widget: QWidgetH; text: PWideString): Integer; overload; cdecl; external QtIntf name 'QToolBox_insertItem';
function QToolBox_insertItem(handle: QToolBoxH; index: Integer; widget: QWidgetH; icon: QIconH; text: PWideString): Integer; overload; cdecl; external QtIntf name 'QToolBox_insertItem2';
procedure QToolBox_removeItem(handle: QToolBoxH; index: Integer); cdecl; external QtIntf name 'QToolBox_removeItem';
procedure QToolBox_setItemEnabled(handle: QToolBoxH; index: Integer; enabled: Boolean); cdecl; external QtIntf name 'QToolBox_setItemEnabled';
function QToolBox_isItemEnabled(handle: QToolBoxH; index: Integer): Boolean; cdecl; external QtIntf name 'QToolBox_isItemEnabled';
procedure QToolBox_setItemText(handle: QToolBoxH; index: Integer; text: PWideString); cdecl; external QtIntf name 'QToolBox_setItemText';
procedure QToolBox_itemText(handle: QToolBoxH; retval: PWideString; index: Integer); cdecl; external QtIntf name 'QToolBox_itemText';
procedure QToolBox_setItemIcon(handle: QToolBoxH; index: Integer; icon: QIconH); cdecl; external QtIntf name 'QToolBox_setItemIcon';
procedure QToolBox_itemIcon(handle: QToolBoxH; retval: QIconH; index: Integer); cdecl; external QtIntf name 'QToolBox_itemIcon';
procedure QToolBox_setItemToolTip(handle: QToolBoxH; index: Integer; toolTip: PWideString); cdecl; external QtIntf name 'QToolBox_setItemToolTip';
procedure QToolBox_itemToolTip(handle: QToolBoxH; retval: PWideString; index: Integer); cdecl; external QtIntf name 'QToolBox_itemToolTip';
function QToolBox_currentIndex(handle: QToolBoxH): Integer; cdecl; external QtIntf name 'QToolBox_currentIndex';
function QToolBox_currentWidget(handle: QToolBoxH): QWidgetH; cdecl; external QtIntf name 'QToolBox_currentWidget';
function QToolBox_widget(handle: QToolBoxH; index: Integer): QWidgetH; cdecl; external QtIntf name 'QToolBox_widget';
function QToolBox_indexOf(handle: QToolBoxH; widget: QWidgetH): Integer; cdecl; external QtIntf name 'QToolBox_indexOf';
function QToolBox_count(handle: QToolBoxH): Integer; cdecl; external QtIntf name 'QToolBox_count';
procedure QToolBox_setCurrentIndex(handle: QToolBoxH; index: Integer); cdecl; external QtIntf name 'QToolBox_setCurrentIndex';
procedure QToolBox_setCurrentWidget(handle: QToolBoxH; widget: QWidgetH); cdecl; external QtIntf name 'QToolBox_setCurrentWidget';


type
  QToolBox_currentChanged_Event = procedure (index: Integer) of object cdecl;



type
  QToolButtonToolButtonPopupMode = ( // QToolButton::ToolButtonPopupMode (1)
    QToolButtonDelayedPopup, QToolButtonMenuButtonPopup, QToolButtonInstantPopup );

function QToolButton_create(parent: QWidgetH = nil): QToolButtonH; cdecl; external QtIntf name 'QToolButton_create';
procedure QToolButton_destroy(handle: QToolButtonH); cdecl; external QtIntf name 'QToolButton_destroy'; 
procedure QToolButton_sizeHint(handle: QToolButtonH; retval: PSize); cdecl; external QtIntf name 'QToolButton_sizeHint';
procedure QToolButton_minimumSizeHint(handle: QToolButtonH; retval: PSize); cdecl; external QtIntf name 'QToolButton_minimumSizeHint';
function QToolButton_toolButtonStyle(handle: QToolButtonH): QtToolButtonStyle; cdecl; external QtIntf name 'QToolButton_toolButtonStyle';
function QToolButton_arrowType(handle: QToolButtonH): QtArrowType; cdecl; external QtIntf name 'QToolButton_arrowType';
procedure QToolButton_setArrowType(handle: QToolButtonH; _type: QtArrowType); cdecl; external QtIntf name 'QToolButton_setArrowType';
procedure QToolButton_setMenu(handle: QToolButtonH; menu: QMenuH); cdecl; external QtIntf name 'QToolButton_setMenu';
function QToolButton_menu(handle: QToolButtonH): QMenuH; cdecl; external QtIntf name 'QToolButton_menu';
procedure QToolButton_setPopupMode(handle: QToolButtonH; mode: QToolButtonToolButtonPopupMode); cdecl; external QtIntf name 'QToolButton_setPopupMode';
function QToolButton_popupMode(handle: QToolButtonH): QToolButtonToolButtonPopupMode; cdecl; external QtIntf name 'QToolButton_popupMode';
function QToolButton_defaultAction(handle: QToolButtonH): QActionH; cdecl; external QtIntf name 'QToolButton_defaultAction';
procedure QToolButton_setAutoRaise(handle: QToolButtonH; enable: Boolean); cdecl; external QtIntf name 'QToolButton_setAutoRaise';
function QToolButton_autoRaise(handle: QToolButtonH): Boolean; cdecl; external QtIntf name 'QToolButton_autoRaise';
procedure QToolButton_showMenu(handle: QToolButtonH); cdecl; external QtIntf name 'QToolButton_showMenu';
procedure QToolButton_setToolButtonStyle(handle: QToolButtonH; style: QtToolButtonStyle); cdecl; external QtIntf name 'QToolButton_setToolButtonStyle';
procedure QToolButton_setDefaultAction(handle: QToolButtonH; p1: QActionH); cdecl; external QtIntf name 'QToolButton_setDefaultAction';


type
  QToolButton_triggered_Event = procedure (p1: QActionH) of object cdecl;


function QScrollArea_create(parent: QWidgetH = nil): QScrollAreaH; cdecl; external QtIntf name 'QScrollArea_create';
procedure QScrollArea_destroy(handle: QScrollAreaH); cdecl; external QtIntf name 'QScrollArea_destroy'; 
function QScrollArea_widget(handle: QScrollAreaH): QWidgetH; cdecl; external QtIntf name 'QScrollArea_widget';
procedure QScrollArea_setWidget(handle: QScrollAreaH; widget: QWidgetH); cdecl; external QtIntf name 'QScrollArea_setWidget';
function QScrollArea_takeWidget(handle: QScrollAreaH): QWidgetH; cdecl; external QtIntf name 'QScrollArea_takeWidget';
function QScrollArea_widgetResizable(handle: QScrollAreaH): Boolean; cdecl; external QtIntf name 'QScrollArea_widgetResizable';
procedure QScrollArea_setWidgetResizable(handle: QScrollAreaH; resizable: Boolean); cdecl; external QtIntf name 'QScrollArea_setWidgetResizable';
procedure QScrollArea_sizeHint(handle: QScrollAreaH; retval: PSize); cdecl; external QtIntf name 'QScrollArea_sizeHint';
function QScrollArea_focusNextPrevChild(handle: QScrollAreaH; next: Boolean): Boolean; cdecl; external QtIntf name 'QScrollArea_focusNextPrevChild';
function QScrollArea_alignment(handle: QScrollAreaH): QtAlignment; cdecl; external QtIntf name 'QScrollArea_alignment';
procedure QScrollArea_setAlignment(handle: QScrollAreaH; p1: QtAlignment); cdecl; external QtIntf name 'QScrollArea_setAlignment';
procedure QScrollArea_ensureVisible(handle: QScrollAreaH; x: Integer; y: Integer; xmargin: Integer = 50; ymargin: Integer = 50); cdecl; external QtIntf name 'QScrollArea_ensureVisible';
procedure QScrollArea_ensureWidgetVisible(handle: QScrollAreaH; childWidget: QWidgetH; xmargin: Integer = 50; ymargin: Integer = 50); cdecl; external QtIntf name 'QScrollArea_ensureWidgetVisible';


type
  QCalendarWidgetHorizontalHeaderFormat = ( // QCalendarWidget::HorizontalHeaderFormat (1)
    QCalendarWidgetNoHorizontalHeader, QCalendarWidgetSingleLetterDayNames, QCalendarWidgetShortDayNames, QCalendarWidgetLongDayNames );

  QCalendarWidgetVerticalHeaderFormat = ( // QCalendarWidget::VerticalHeaderFormat (1)
    QCalendarWidgetNoVerticalHeader, QCalendarWidgetISOWeekNumbers );

  QCalendarWidgetSelectionMode = ( // QCalendarWidget::SelectionMode (1)
    QCalendarWidgetNoSelection, QCalendarWidgetSingleSelection );

function QCalendarWidget_create(parent: QWidgetH = nil): QCalendarWidgetH; cdecl; external QtIntf name 'QCalendarWidget_create';
procedure QCalendarWidget_destroy(handle: QCalendarWidgetH); cdecl; external QtIntf name 'QCalendarWidget_destroy'; 
procedure QCalendarWidget_sizeHint(handle: QCalendarWidgetH; retval: PSize); cdecl; external QtIntf name 'QCalendarWidget_sizeHint';
procedure QCalendarWidget_minimumSizeHint(handle: QCalendarWidgetH; retval: PSize); cdecl; external QtIntf name 'QCalendarWidget_minimumSizeHint';
procedure QCalendarWidget_selectedDate(handle: QCalendarWidgetH; retval: QDateH); cdecl; external QtIntf name 'QCalendarWidget_selectedDate';
function QCalendarWidget_yearShown(handle: QCalendarWidgetH): Integer; cdecl; external QtIntf name 'QCalendarWidget_yearShown';
function QCalendarWidget_monthShown(handle: QCalendarWidgetH): Integer; cdecl; external QtIntf name 'QCalendarWidget_monthShown';
procedure QCalendarWidget_minimumDate(handle: QCalendarWidgetH; retval: QDateH); cdecl; external QtIntf name 'QCalendarWidget_minimumDate';
procedure QCalendarWidget_setMinimumDate(handle: QCalendarWidgetH; date: QDateH); cdecl; external QtIntf name 'QCalendarWidget_setMinimumDate';
procedure QCalendarWidget_maximumDate(handle: QCalendarWidgetH; retval: QDateH); cdecl; external QtIntf name 'QCalendarWidget_maximumDate';
procedure QCalendarWidget_setMaximumDate(handle: QCalendarWidgetH; date: QDateH); cdecl; external QtIntf name 'QCalendarWidget_setMaximumDate';
function QCalendarWidget_firstDayOfWeek(handle: QCalendarWidgetH): QtDayOfWeek; cdecl; external QtIntf name 'QCalendarWidget_firstDayOfWeek';
procedure QCalendarWidget_setFirstDayOfWeek(handle: QCalendarWidgetH; dayOfWeek: QtDayOfWeek); cdecl; external QtIntf name 'QCalendarWidget_setFirstDayOfWeek';
function QCalendarWidget_isHeaderVisible(handle: QCalendarWidgetH): Boolean; cdecl; external QtIntf name 'QCalendarWidget_isHeaderVisible';
procedure QCalendarWidget_setHeaderVisible(handle: QCalendarWidgetH; show: Boolean); cdecl; external QtIntf name 'QCalendarWidget_setHeaderVisible';
function QCalendarWidget_isGridVisible(handle: QCalendarWidgetH): Boolean; cdecl; external QtIntf name 'QCalendarWidget_isGridVisible';
procedure QCalendarWidget_setGridVisible(handle: QCalendarWidgetH; show: Boolean); cdecl; external QtIntf name 'QCalendarWidget_setGridVisible';
function QCalendarWidget_selectionMode(handle: QCalendarWidgetH): QCalendarWidgetSelectionMode; cdecl; external QtIntf name 'QCalendarWidget_selectionMode';
procedure QCalendarWidget_setSelectionMode(handle: QCalendarWidgetH; mode: QCalendarWidgetSelectionMode); cdecl; external QtIntf name 'QCalendarWidget_setSelectionMode';
function QCalendarWidget_horizontalHeaderFormat(handle: QCalendarWidgetH): QCalendarWidgetHorizontalHeaderFormat; cdecl; external QtIntf name 'QCalendarWidget_horizontalHeaderFormat';
procedure QCalendarWidget_setHorizontalHeaderFormat(handle: QCalendarWidgetH; format: QCalendarWidgetHorizontalHeaderFormat); cdecl; external QtIntf name 'QCalendarWidget_setHorizontalHeaderFormat';
function QCalendarWidget_verticalHeaderFormat(handle: QCalendarWidgetH): QCalendarWidgetVerticalHeaderFormat; cdecl; external QtIntf name 'QCalendarWidget_verticalHeaderFormat';
procedure QCalendarWidget_setVerticalHeaderFormat(handle: QCalendarWidgetH; format: QCalendarWidgetVerticalHeaderFormat); cdecl; external QtIntf name 'QCalendarWidget_setVerticalHeaderFormat';
procedure QCalendarWidget_headerTextFormat(handle: QCalendarWidgetH; retval: QTextCharFormatH); cdecl; external QtIntf name 'QCalendarWidget_headerTextFormat';
procedure QCalendarWidget_setHeaderTextFormat(handle: QCalendarWidgetH; format: QTextCharFormatH); cdecl; external QtIntf name 'QCalendarWidget_setHeaderTextFormat';
procedure QCalendarWidget_weekdayTextFormat(handle: QCalendarWidgetH; retval: QTextCharFormatH; dayOfWeek: QtDayOfWeek); cdecl; external QtIntf name 'QCalendarWidget_weekdayTextFormat';
procedure QCalendarWidget_setWeekdayTextFormat(handle: QCalendarWidgetH; dayOfWeek: QtDayOfWeek; format: QTextCharFormatH); cdecl; external QtIntf name 'QCalendarWidget_setWeekdayTextFormat';
procedure QCalendarWidget_dateTextFormat(handle: QCalendarWidgetH; retval: QTextCharFormatH; date: QDateH); overload; cdecl; external QtIntf name 'QCalendarWidget_dateTextFormat2';
procedure QCalendarWidget_setDateTextFormat(handle: QCalendarWidgetH; date: QDateH; color: QTextCharFormatH); cdecl; external QtIntf name 'QCalendarWidget_setDateTextFormat';
procedure QCalendarWidget_setSelectedDate(handle: QCalendarWidgetH; date: QDateH); cdecl; external QtIntf name 'QCalendarWidget_setSelectedDate';
procedure QCalendarWidget_setDateRange(handle: QCalendarWidgetH; min: QDateH; max: QDateH); cdecl; external QtIntf name 'QCalendarWidget_setDateRange';
procedure QCalendarWidget_setCurrentPage(handle: QCalendarWidgetH; year: Integer; month: Integer); cdecl; external QtIntf name 'QCalendarWidget_setCurrentPage';
procedure QCalendarWidget_showNextMonth(handle: QCalendarWidgetH); cdecl; external QtIntf name 'QCalendarWidget_showNextMonth';
procedure QCalendarWidget_showPreviousMonth(handle: QCalendarWidgetH); cdecl; external QtIntf name 'QCalendarWidget_showPreviousMonth';
procedure QCalendarWidget_showNextYear(handle: QCalendarWidgetH); cdecl; external QtIntf name 'QCalendarWidget_showNextYear';
procedure QCalendarWidget_showPreviousYear(handle: QCalendarWidgetH); cdecl; external QtIntf name 'QCalendarWidget_showPreviousYear';
procedure QCalendarWidget_showSelectedDate(handle: QCalendarWidgetH); cdecl; external QtIntf name 'QCalendarWidget_showSelectedDate';
procedure QCalendarWidget_showToday(handle: QCalendarWidgetH); cdecl; external QtIntf name 'QCalendarWidget_showToday';


type
  QCalendarWidget_selectionChanged_Event = procedure () of object cdecl;
  QCalendarWidget_clicked_Event = procedure (date: QDateH) of object cdecl;
  QCalendarWidget_activated_Event = procedure (date: QDateH) of object cdecl;
  QCalendarWidget_currentPageChanged_Event = procedure (year: Integer; month: Integer) of object cdecl;



type
  QRubberBandShape = ( // QRubberBand::Shape (1)
    QRubberBandLine, QRubberBandRectangle );

function QRubberBand_create(p1: QRubberBandShape; p2: QWidgetH = nil): QRubberBandH; cdecl; external QtIntf name 'QRubberBand_create';
procedure QRubberBand_destroy(handle: QRubberBandH); cdecl; external QtIntf name 'QRubberBand_destroy'; 
function QRubberBand_shape(handle: QRubberBandH): QRubberBandShape; cdecl; external QtIntf name 'QRubberBand_shape';
procedure QRubberBand_setGeometry(handle: QRubberBandH; r: PRect); overload; cdecl; external QtIntf name 'QRubberBand_setGeometry';
procedure QRubberBand_setGeometry(handle: QRubberBandH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QRubberBand_setGeometry2';
procedure QRubberBand_move(handle: QRubberBandH; x: Integer; y: Integer); overload; cdecl; external QtIntf name 'QRubberBand_move';
procedure QRubberBand_move(handle: QRubberBandH; p: PQtPoint); overload; cdecl; external QtIntf name 'QRubberBand_move2';
procedure QRubberBand_resize(handle: QRubberBandH; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QRubberBand_resize';
procedure QRubberBand_resize(handle: QRubberBandH; s: PSize); overload; cdecl; external QtIntf name 'QRubberBand_resize2';


type
  QAbstractItemViewSelectionMode = ( // QAbstractItemView::SelectionMode (1)
    QAbstractItemViewNoSelection, QAbstractItemViewSingleSelection, QAbstractItemViewMultiSelection, QAbstractItemViewExtendedSelection, QAbstractItemViewContiguousSelection );

  QAbstractItemViewSelectionBehavior = ( // QAbstractItemView::SelectionBehavior (1)
    QAbstractItemViewSelectItems, QAbstractItemViewSelectRows, QAbstractItemViewSelectColumns );

  QAbstractItemViewScrollHint = ( // QAbstractItemView::ScrollHint (1)
    QAbstractItemViewEnsureVisible, QAbstractItemViewPositionAtTop, QAbstractItemViewPositionAtBottom, QAbstractItemViewPositionAtCenter );

  QAbstractItemViewScrollMode = ( // QAbstractItemView::ScrollMode (1)
    QAbstractItemViewScrollPerItem, QAbstractItemViewScrollPerPixel );

  QAbstractItemViewDragDropMode = ( // QAbstractItemView::DragDropMode (1)
    QAbstractItemViewNoDragDrop, QAbstractItemViewDragOnly, QAbstractItemViewDropOnly, QAbstractItemViewDragDrop, QAbstractItemViewInternalMove );

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

procedure QAbstractItemView_setModel(handle: QAbstractItemViewH; model: QAbstractItemModelH); cdecl; external QtIntf name 'QAbstractItemView_setModel';
function QAbstractItemView_model(handle: QAbstractItemViewH): QAbstractItemModelH; cdecl; external QtIntf name 'QAbstractItemView_model';
procedure QAbstractItemView_setSelectionModel(handle: QAbstractItemViewH; selectionModel: QItemSelectionModelH); cdecl; external QtIntf name 'QAbstractItemView_setSelectionModel';
function QAbstractItemView_selectionModel(handle: QAbstractItemViewH): QItemSelectionModelH; cdecl; external QtIntf name 'QAbstractItemView_selectionModel';
procedure QAbstractItemView_setItemDelegate(handle: QAbstractItemViewH; delegate: QAbstractItemDelegateH); cdecl; external QtIntf name 'QAbstractItemView_setItemDelegate';
function QAbstractItemView_itemDelegate(handle: QAbstractItemViewH): QAbstractItemDelegateH; overload; cdecl; external QtIntf name 'QAbstractItemView_itemDelegate';
procedure QAbstractItemView_setSelectionMode(handle: QAbstractItemViewH; mode: QAbstractItemViewSelectionMode); cdecl; external QtIntf name 'QAbstractItemView_setSelectionMode';
function QAbstractItemView_selectionMode(handle: QAbstractItemViewH): QAbstractItemViewSelectionMode; cdecl; external QtIntf name 'QAbstractItemView_selectionMode';
procedure QAbstractItemView_setSelectionBehavior(handle: QAbstractItemViewH; behavior: QAbstractItemViewSelectionBehavior); cdecl; external QtIntf name 'QAbstractItemView_setSelectionBehavior';
function QAbstractItemView_selectionBehavior(handle: QAbstractItemViewH): QAbstractItemViewSelectionBehavior; cdecl; external QtIntf name 'QAbstractItemView_selectionBehavior';
procedure QAbstractItemView_currentIndex(handle: QAbstractItemViewH; retval: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemView_currentIndex';
procedure QAbstractItemView_rootIndex(handle: QAbstractItemViewH; retval: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemView_rootIndex';
procedure QAbstractItemView_setEditTriggers(handle: QAbstractItemViewH; triggers: QAbstractItemViewEditTriggers); cdecl; external QtIntf name 'QAbstractItemView_setEditTriggers';
function QAbstractItemView_editTriggers(handle: QAbstractItemViewH): QAbstractItemViewEditTriggers; cdecl; external QtIntf name 'QAbstractItemView_editTriggers';
procedure QAbstractItemView_setVerticalScrollMode(handle: QAbstractItemViewH; mode: QAbstractItemViewScrollMode); cdecl; external QtIntf name 'QAbstractItemView_setVerticalScrollMode';
function QAbstractItemView_verticalScrollMode(handle: QAbstractItemViewH): QAbstractItemViewScrollMode; cdecl; external QtIntf name 'QAbstractItemView_verticalScrollMode';
procedure QAbstractItemView_setHorizontalScrollMode(handle: QAbstractItemViewH; mode: QAbstractItemViewScrollMode); cdecl; external QtIntf name 'QAbstractItemView_setHorizontalScrollMode';
function QAbstractItemView_horizontalScrollMode(handle: QAbstractItemViewH): QAbstractItemViewScrollMode; cdecl; external QtIntf name 'QAbstractItemView_horizontalScrollMode';
procedure QAbstractItemView_setAutoScroll(handle: QAbstractItemViewH; enable: Boolean); cdecl; external QtIntf name 'QAbstractItemView_setAutoScroll';
function QAbstractItemView_hasAutoScroll(handle: QAbstractItemViewH): Boolean; cdecl; external QtIntf name 'QAbstractItemView_hasAutoScroll';
procedure QAbstractItemView_setTabKeyNavigation(handle: QAbstractItemViewH; enable: Boolean); cdecl; external QtIntf name 'QAbstractItemView_setTabKeyNavigation';
function QAbstractItemView_tabKeyNavigation(handle: QAbstractItemViewH): Boolean; cdecl; external QtIntf name 'QAbstractItemView_tabKeyNavigation';
procedure QAbstractItemView_setDropIndicatorShown(handle: QAbstractItemViewH; enable: Boolean); cdecl; external QtIntf name 'QAbstractItemView_setDropIndicatorShown';
function QAbstractItemView_showDropIndicator(handle: QAbstractItemViewH): Boolean; cdecl; external QtIntf name 'QAbstractItemView_showDropIndicator';
procedure QAbstractItemView_setDragEnabled(handle: QAbstractItemViewH; enable: Boolean); cdecl; external QtIntf name 'QAbstractItemView_setDragEnabled';
function QAbstractItemView_dragEnabled(handle: QAbstractItemViewH): Boolean; cdecl; external QtIntf name 'QAbstractItemView_dragEnabled';
procedure QAbstractItemView_setDragDropOverwriteMode(handle: QAbstractItemViewH; overwrite: Boolean); cdecl; external QtIntf name 'QAbstractItemView_setDragDropOverwriteMode';
function QAbstractItemView_dragDropOverwriteMode(handle: QAbstractItemViewH): Boolean; cdecl; external QtIntf name 'QAbstractItemView_dragDropOverwriteMode';
procedure QAbstractItemView_setDragDropMode(handle: QAbstractItemViewH; behavior: QAbstractItemViewDragDropMode); cdecl; external QtIntf name 'QAbstractItemView_setDragDropMode';
function QAbstractItemView_dragDropMode(handle: QAbstractItemViewH): QAbstractItemViewDragDropMode; cdecl; external QtIntf name 'QAbstractItemView_dragDropMode';
procedure QAbstractItemView_setAlternatingRowColors(handle: QAbstractItemViewH; enable: Boolean); cdecl; external QtIntf name 'QAbstractItemView_setAlternatingRowColors';
function QAbstractItemView_alternatingRowColors(handle: QAbstractItemViewH): Boolean; cdecl; external QtIntf name 'QAbstractItemView_alternatingRowColors';
procedure QAbstractItemView_setIconSize(handle: QAbstractItemViewH; size: PSize); cdecl; external QtIntf name 'QAbstractItemView_setIconSize';
procedure QAbstractItemView_iconSize(handle: QAbstractItemViewH; retval: PSize); cdecl; external QtIntf name 'QAbstractItemView_iconSize';
procedure QAbstractItemView_setTextElideMode(handle: QAbstractItemViewH; mode: QtTextElideMode); cdecl; external QtIntf name 'QAbstractItemView_setTextElideMode';
function QAbstractItemView_textElideMode(handle: QAbstractItemViewH): QtTextElideMode; cdecl; external QtIntf name 'QAbstractItemView_textElideMode';
procedure QAbstractItemView_keyboardSearch(handle: QAbstractItemViewH; search: PWideString); cdecl; external QtIntf name 'QAbstractItemView_keyboardSearch';
procedure QAbstractItemView_visualRect(handle: QAbstractItemViewH; retval: PRect; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemView_visualRect';
procedure QAbstractItemView_scrollTo(handle: QAbstractItemViewH; index: QModelIndexH; hint: QAbstractItemViewScrollHint = QAbstractItemViewEnsureVisible); cdecl; external QtIntf name 'QAbstractItemView_scrollTo';
procedure QAbstractItemView_indexAt(handle: QAbstractItemViewH; retval: QModelIndexH; point: PQtPoint); cdecl; external QtIntf name 'QAbstractItemView_indexAt';
procedure QAbstractItemView_sizeHintForIndex(handle: QAbstractItemViewH; retval: PSize; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemView_sizeHintForIndex';
function QAbstractItemView_sizeHintForRow(handle: QAbstractItemViewH; row: Integer): Integer; cdecl; external QtIntf name 'QAbstractItemView_sizeHintForRow';
function QAbstractItemView_sizeHintForColumn(handle: QAbstractItemViewH; column: Integer): Integer; cdecl; external QtIntf name 'QAbstractItemView_sizeHintForColumn';
procedure QAbstractItemView_openPersistentEditor(handle: QAbstractItemViewH; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemView_openPersistentEditor';
procedure QAbstractItemView_closePersistentEditor(handle: QAbstractItemViewH; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemView_closePersistentEditor';
procedure QAbstractItemView_setIndexWidget(handle: QAbstractItemViewH; index: QModelIndexH; widget: QWidgetH); cdecl; external QtIntf name 'QAbstractItemView_setIndexWidget';
function QAbstractItemView_indexWidget(handle: QAbstractItemViewH; index: QModelIndexH): QWidgetH; cdecl; external QtIntf name 'QAbstractItemView_indexWidget';
procedure QAbstractItemView_setItemDelegateForRow(handle: QAbstractItemViewH; row: Integer; delegate: QAbstractItemDelegateH); cdecl; external QtIntf name 'QAbstractItemView_setItemDelegateForRow';
function QAbstractItemView_itemDelegateForRow(handle: QAbstractItemViewH; row: Integer): QAbstractItemDelegateH; cdecl; external QtIntf name 'QAbstractItemView_itemDelegateForRow';
procedure QAbstractItemView_setItemDelegateForColumn(handle: QAbstractItemViewH; column: Integer; delegate: QAbstractItemDelegateH); cdecl; external QtIntf name 'QAbstractItemView_setItemDelegateForColumn';
function QAbstractItemView_itemDelegateForColumn(handle: QAbstractItemViewH; column: Integer): QAbstractItemDelegateH; cdecl; external QtIntf name 'QAbstractItemView_itemDelegateForColumn';
function QAbstractItemView_itemDelegate(handle: QAbstractItemViewH; index: QModelIndexH): QAbstractItemDelegateH; overload; cdecl; external QtIntf name 'QAbstractItemView_itemDelegate2';
procedure QAbstractItemView_inputMethodQuery(handle: QAbstractItemViewH; retval: QVariantH; query: QtInputMethodQuery); cdecl; external QtIntf name 'QAbstractItemView_inputMethodQuery';
procedure QAbstractItemView_reset(handle: QAbstractItemViewH); cdecl; external QtIntf name 'QAbstractItemView_reset';
procedure QAbstractItemView_setRootIndex(handle: QAbstractItemViewH; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemView_setRootIndex';
procedure QAbstractItemView_doItemsLayout(handle: QAbstractItemViewH); cdecl; external QtIntf name 'QAbstractItemView_doItemsLayout';
procedure QAbstractItemView_selectAll(handle: QAbstractItemViewH); cdecl; external QtIntf name 'QAbstractItemView_selectAll';
procedure QAbstractItemView_edit(handle: QAbstractItemViewH; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemView_edit';
procedure QAbstractItemView_clearSelection(handle: QAbstractItemViewH); cdecl; external QtIntf name 'QAbstractItemView_clearSelection';
procedure QAbstractItemView_setCurrentIndex(handle: QAbstractItemViewH; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemView_setCurrentIndex';
procedure QAbstractItemView_scrollToTop(handle: QAbstractItemViewH); cdecl; external QtIntf name 'QAbstractItemView_scrollToTop';
procedure QAbstractItemView_scrollToBottom(handle: QAbstractItemViewH); cdecl; external QtIntf name 'QAbstractItemView_scrollToBottom';


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

function QListView_create(parent: QWidgetH = nil): QListViewH; cdecl; external QtIntf name 'QListView_create';
procedure QListView_destroy(handle: QListViewH); cdecl; external QtIntf name 'QListView_destroy'; 
procedure QListView_setMovement(handle: QListViewH; movement: QListViewMovement); cdecl; external QtIntf name 'QListView_setMovement';
function QListView_movement(handle: QListViewH): QListViewMovement; cdecl; external QtIntf name 'QListView_movement';
procedure QListView_setFlow(handle: QListViewH; flow: QListViewFlow); cdecl; external QtIntf name 'QListView_setFlow';
function QListView_flow(handle: QListViewH): QListViewFlow; cdecl; external QtIntf name 'QListView_flow';
procedure QListView_setWrapping(handle: QListViewH; enable: Boolean); cdecl; external QtIntf name 'QListView_setWrapping';
function QListView_isWrapping(handle: QListViewH): Boolean; cdecl; external QtIntf name 'QListView_isWrapping';
procedure QListView_setResizeMode(handle: QListViewH; mode: QListViewResizeMode); cdecl; external QtIntf name 'QListView_setResizeMode';
function QListView_resizeMode(handle: QListViewH): QListViewResizeMode; cdecl; external QtIntf name 'QListView_resizeMode';
procedure QListView_setLayoutMode(handle: QListViewH; mode: QListViewLayoutMode); cdecl; external QtIntf name 'QListView_setLayoutMode';
function QListView_layoutMode(handle: QListViewH): QListViewLayoutMode; cdecl; external QtIntf name 'QListView_layoutMode';
procedure QListView_setSpacing(handle: QListViewH; space: Integer); cdecl; external QtIntf name 'QListView_setSpacing';
function QListView_spacing(handle: QListViewH): Integer; cdecl; external QtIntf name 'QListView_spacing';
procedure QListView_setBatchSize(handle: QListViewH; batchSize: Integer); cdecl; external QtIntf name 'QListView_setBatchSize';
function QListView_batchSize(handle: QListViewH): Integer; cdecl; external QtIntf name 'QListView_batchSize';
procedure QListView_setGridSize(handle: QListViewH; size: PSize); cdecl; external QtIntf name 'QListView_setGridSize';
procedure QListView_gridSize(handle: QListViewH; retval: PSize); cdecl; external QtIntf name 'QListView_gridSize';
procedure QListView_setViewMode(handle: QListViewH; mode: QListViewViewMode); cdecl; external QtIntf name 'QListView_setViewMode';
function QListView_viewMode(handle: QListViewH): QListViewViewMode; cdecl; external QtIntf name 'QListView_viewMode';
procedure QListView_clearPropertyFlags(handle: QListViewH); cdecl; external QtIntf name 'QListView_clearPropertyFlags';
function QListView_isRowHidden(handle: QListViewH; row: Integer): Boolean; cdecl; external QtIntf name 'QListView_isRowHidden';
procedure QListView_setRowHidden(handle: QListViewH; row: Integer; hide: Boolean); cdecl; external QtIntf name 'QListView_setRowHidden';
procedure QListView_setModelColumn(handle: QListViewH; column: Integer); cdecl; external QtIntf name 'QListView_setModelColumn';
function QListView_modelColumn(handle: QListViewH): Integer; cdecl; external QtIntf name 'QListView_modelColumn';
procedure QListView_setUniformItemSizes(handle: QListViewH; enable: Boolean); cdecl; external QtIntf name 'QListView_setUniformItemSizes';
function QListView_uniformItemSizes(handle: QListViewH): Boolean; cdecl; external QtIntf name 'QListView_uniformItemSizes';
procedure QListView_setWordWrap(handle: QListViewH; _on: Boolean); cdecl; external QtIntf name 'QListView_setWordWrap';
function QListView_wordWrap(handle: QListViewH): Boolean; cdecl; external QtIntf name 'QListView_wordWrap';
procedure QListView_visualRect(handle: QListViewH; retval: PRect; index: QModelIndexH); cdecl; external QtIntf name 'QListView_visualRect';
procedure QListView_scrollTo(handle: QListViewH; index: QModelIndexH; hint: QAbstractItemViewScrollHint); cdecl; external QtIntf name 'QListView_scrollTo';
procedure QListView_indexAt(handle: QListViewH; retval: QModelIndexH; p: PQtPoint); cdecl; external QtIntf name 'QListView_indexAt';
procedure QListView_doItemsLayout(handle: QListViewH); cdecl; external QtIntf name 'QListView_doItemsLayout';
procedure QListView_reset(handle: QListViewH); cdecl; external QtIntf name 'QListView_reset';
procedure QListView_setRootIndex(handle: QListViewH; index: QModelIndexH); cdecl; external QtIntf name 'QListView_setRootIndex';


type
  QListWidgetItemItemType = (  //QListWidgetItem::ItemType (2s)
    QListWidgetItemType = 0,
    QListWidgetItemUserType = 1000 );

function QListWidgetItem_create(view: QListWidgetH = nil; _type: Integer = QListWidgetItemType): QListWidgetItemH; overload; cdecl; external QtIntf name 'QListWidgetItem_create';
procedure QListWidgetItem_destroy(handle: QListWidgetItemH); cdecl; external QtIntf name 'QListWidgetItem_destroy'; 
function QListWidgetItem_create(text: PWideString; view: QListWidgetH = nil; _type: Integer = QListWidgetItemType): QListWidgetItemH; overload; cdecl; external QtIntf name 'QListWidgetItem_create2';
function QListWidgetItem_create(icon: QIconH; text: PWideString; view: QListWidgetH = nil; _type: Integer = QListWidgetItemType): QListWidgetItemH; overload; cdecl; external QtIntf name 'QListWidgetItem_create3';
function QListWidgetItem_create(other: QListWidgetItemH): QListWidgetItemH; overload; cdecl; external QtIntf name 'QListWidgetItem_create4';
function QListWidgetItem_clone(handle: QListWidgetItemH): QListWidgetItemH; cdecl; external QtIntf name 'QListWidgetItem_clone';
function QListWidgetItem_listWidget(handle: QListWidgetItemH): QListWidgetH; cdecl; external QtIntf name 'QListWidgetItem_listWidget';
procedure QListWidgetItem_setSelected(handle: QListWidgetItemH; select: Boolean); cdecl; external QtIntf name 'QListWidgetItem_setSelected';
function QListWidgetItem_isSelected(handle: QListWidgetItemH): Boolean; cdecl; external QtIntf name 'QListWidgetItem_isSelected';
procedure QListWidgetItem_setHidden(handle: QListWidgetItemH; hide: Boolean); cdecl; external QtIntf name 'QListWidgetItem_setHidden';
function QListWidgetItem_isHidden(handle: QListWidgetItemH): Boolean; cdecl; external QtIntf name 'QListWidgetItem_isHidden';
function QListWidgetItem_flags(handle: QListWidgetItemH): QtItemFlags; cdecl; external QtIntf name 'QListWidgetItem_flags';
procedure QListWidgetItem_setFlags(handle: QListWidgetItemH; flags: QtItemFlags); cdecl; external QtIntf name 'QListWidgetItem_setFlags';
procedure QListWidgetItem_text(handle: QListWidgetItemH; retval: PWideString); cdecl; external QtIntf name 'QListWidgetItem_text';
procedure QListWidgetItem_setText(handle: QListWidgetItemH; text: PWideString); cdecl; external QtIntf name 'QListWidgetItem_setText';
procedure QListWidgetItem_icon(handle: QListWidgetItemH; retval: QIconH); cdecl; external QtIntf name 'QListWidgetItem_icon';
procedure QListWidgetItem_setIcon(handle: QListWidgetItemH; icon: QIconH); cdecl; external QtIntf name 'QListWidgetItem_setIcon';
procedure QListWidgetItem_statusTip(handle: QListWidgetItemH; retval: PWideString); cdecl; external QtIntf name 'QListWidgetItem_statusTip';
procedure QListWidgetItem_setStatusTip(handle: QListWidgetItemH; statusTip: PWideString); cdecl; external QtIntf name 'QListWidgetItem_setStatusTip';
procedure QListWidgetItem_toolTip(handle: QListWidgetItemH; retval: PWideString); cdecl; external QtIntf name 'QListWidgetItem_toolTip';
procedure QListWidgetItem_setToolTip(handle: QListWidgetItemH; toolTip: PWideString); cdecl; external QtIntf name 'QListWidgetItem_setToolTip';
procedure QListWidgetItem_whatsThis(handle: QListWidgetItemH; retval: PWideString); cdecl; external QtIntf name 'QListWidgetItem_whatsThis';
procedure QListWidgetItem_setWhatsThis(handle: QListWidgetItemH; whatsThis: PWideString); cdecl; external QtIntf name 'QListWidgetItem_setWhatsThis';
procedure QListWidgetItem_font(handle: QListWidgetItemH; retval: QFontH); cdecl; external QtIntf name 'QListWidgetItem_font';
procedure QListWidgetItem_setFont(handle: QListWidgetItemH; font: QFontH); cdecl; external QtIntf name 'QListWidgetItem_setFont';
function QListWidgetItem_textAlignment(handle: QListWidgetItemH): Integer; cdecl; external QtIntf name 'QListWidgetItem_textAlignment';
procedure QListWidgetItem_setTextAlignment(handle: QListWidgetItemH; alignment: Integer); cdecl; external QtIntf name 'QListWidgetItem_setTextAlignment';
procedure QListWidgetItem_backgroundColor(handle: QListWidgetItemH; retval: PQColor); cdecl; external QtIntf name 'QListWidgetItem_backgroundColor';
procedure QListWidgetItem_setBackgroundColor(handle: QListWidgetItemH; color: PQColor); cdecl; external QtIntf name 'QListWidgetItem_setBackgroundColor';
procedure QListWidgetItem_background(handle: QListWidgetItemH; retval: QBrushH); cdecl; external QtIntf name 'QListWidgetItem_background';
procedure QListWidgetItem_setBackground(handle: QListWidgetItemH; brush: QBrushH); cdecl; external QtIntf name 'QListWidgetItem_setBackground';
procedure QListWidgetItem_textColor(handle: QListWidgetItemH; retval: PQColor); cdecl; external QtIntf name 'QListWidgetItem_textColor';
procedure QListWidgetItem_setTextColor(handle: QListWidgetItemH; color: PQColor); cdecl; external QtIntf name 'QListWidgetItem_setTextColor';
procedure QListWidgetItem_foreground(handle: QListWidgetItemH; retval: QBrushH); cdecl; external QtIntf name 'QListWidgetItem_foreground';
procedure QListWidgetItem_setForeground(handle: QListWidgetItemH; brush: QBrushH); cdecl; external QtIntf name 'QListWidgetItem_setForeground';
function QListWidgetItem_checkState(handle: QListWidgetItemH): QtCheckState; cdecl; external QtIntf name 'QListWidgetItem_checkState';
procedure QListWidgetItem_setCheckState(handle: QListWidgetItemH; state: QtCheckState); cdecl; external QtIntf name 'QListWidgetItem_setCheckState';
procedure QListWidgetItem_sizeHint(handle: QListWidgetItemH; retval: PSize); cdecl; external QtIntf name 'QListWidgetItem_sizeHint';
procedure QListWidgetItem_setSizeHint(handle: QListWidgetItemH; size: PSize); cdecl; external QtIntf name 'QListWidgetItem_setSizeHint';
procedure QListWidgetItem_data(handle: QListWidgetItemH; retval: QVariantH; role: Integer); cdecl; external QtIntf name 'QListWidgetItem_data';
procedure QListWidgetItem_setData(handle: QListWidgetItemH; role: Integer; value: QVariantH); cdecl; external QtIntf name 'QListWidgetItem_setData';
procedure QListWidgetItem_read(handle: QListWidgetItemH; _in: QDataStreamH); cdecl; external QtIntf name 'QListWidgetItem_read';
procedure QListWidgetItem_write(handle: QListWidgetItemH; _out: QDataStreamH); cdecl; external QtIntf name 'QListWidgetItem_write';
function QListWidgetItem_type(handle: QListWidgetItemH): Integer; cdecl; external QtIntf name 'QListWidgetItem_type';

function QListWidget_create(parent: QWidgetH = nil): QListWidgetH; cdecl; external QtIntf name 'QListWidget_create';
procedure QListWidget_destroy(handle: QListWidgetH); cdecl; external QtIntf name 'QListWidget_destroy'; 
function QListWidget_item(handle: QListWidgetH; row: Integer): QListWidgetItemH; cdecl; external QtIntf name 'QListWidget_item';
function QListWidget_row(handle: QListWidgetH; item: QListWidgetItemH): Integer; cdecl; external QtIntf name 'QListWidget_row';
procedure QListWidget_insertItem(handle: QListWidgetH; row: Integer; item: QListWidgetItemH); overload; cdecl; external QtIntf name 'QListWidget_insertItem';
procedure QListWidget_insertItem(handle: QListWidgetH; row: Integer; _label: PWideString); overload; cdecl; external QtIntf name 'QListWidget_insertItem2';
procedure QListWidget_insertItems(handle: QListWidgetH; row: Integer; labels: QStringListH); cdecl; external QtIntf name 'QListWidget_insertItems';
procedure QListWidget_addItem(handle: QListWidgetH; _label: PWideString); overload; cdecl; external QtIntf name 'QListWidget_addItem';
procedure QListWidget_addItem(handle: QListWidgetH; item: QListWidgetItemH); overload; cdecl; external QtIntf name 'QListWidget_addItem2';
procedure QListWidget_addItems(handle: QListWidgetH; labels: QStringListH); cdecl; external QtIntf name 'QListWidget_addItems';
function QListWidget_takeItem(handle: QListWidgetH; row: Integer): QListWidgetItemH; cdecl; external QtIntf name 'QListWidget_takeItem';
function QListWidget_count(handle: QListWidgetH): Integer; cdecl; external QtIntf name 'QListWidget_count';
function QListWidget_currentItem(handle: QListWidgetH): QListWidgetItemH; cdecl; external QtIntf name 'QListWidget_currentItem';
procedure QListWidget_setCurrentItem(handle: QListWidgetH; item: QListWidgetItemH); cdecl; external QtIntf name 'QListWidget_setCurrentItem';
function QListWidget_currentRow(handle: QListWidgetH): Integer; cdecl; external QtIntf name 'QListWidget_currentRow';
procedure QListWidget_setCurrentRow(handle: QListWidgetH; row: Integer); cdecl; external QtIntf name 'QListWidget_setCurrentRow';
function QListWidget_itemAt(handle: QListWidgetH; p: PQtPoint): QListWidgetItemH; overload; cdecl; external QtIntf name 'QListWidget_itemAt';
function QListWidget_itemAt(handle: QListWidgetH; x: Integer; y: Integer): QListWidgetItemH; overload; cdecl; external QtIntf name 'QListWidget_itemAt2';
procedure QListWidget_visualItemRect(handle: QListWidgetH; retval: PRect; item: QListWidgetItemH); cdecl; external QtIntf name 'QListWidget_visualItemRect';
procedure QListWidget_sortItems(handle: QListWidgetH; order: QtSortOrder = QtAscendingOrder); cdecl; external QtIntf name 'QListWidget_sortItems';
procedure QListWidget_setSortingEnabled(handle: QListWidgetH; enable: Boolean); cdecl; external QtIntf name 'QListWidget_setSortingEnabled';
function QListWidget_isSortingEnabled(handle: QListWidgetH): Boolean; cdecl; external QtIntf name 'QListWidget_isSortingEnabled';
procedure QListWidget_editItem(handle: QListWidgetH; item: QListWidgetItemH); cdecl; external QtIntf name 'QListWidget_editItem';
procedure QListWidget_openPersistentEditor(handle: QListWidgetH; item: QListWidgetItemH); cdecl; external QtIntf name 'QListWidget_openPersistentEditor';
procedure QListWidget_closePersistentEditor(handle: QListWidgetH; item: QListWidgetItemH); cdecl; external QtIntf name 'QListWidget_closePersistentEditor';
function QListWidget_itemWidget(handle: QListWidgetH; item: QListWidgetItemH): QWidgetH; cdecl; external QtIntf name 'QListWidget_itemWidget';
procedure QListWidget_setItemWidget(handle: QListWidgetH; item: QListWidgetItemH; widget: QWidgetH); cdecl; external QtIntf name 'QListWidget_setItemWidget';
function QListWidget_isItemSelected(handle: QListWidgetH; item: QListWidgetItemH): Boolean; cdecl; external QtIntf name 'QListWidget_isItemSelected';
procedure QListWidget_setItemSelected(handle: QListWidgetH; item: QListWidgetItemH; select: Boolean); cdecl; external QtIntf name 'QListWidget_setItemSelected';
procedure QListWidget_selectedItems(handle: QListWidgetH; retval: PIntArray); cdecl; external QtIntf name 'QListWidget_selectedItems';
procedure QListWidget_findItems(handle: QListWidgetH; retval: PIntArray; text: PWideString; flags: QtMatchFlags); cdecl; external QtIntf name 'QListWidget_findItems';
function QListWidget_isItemHidden(handle: QListWidgetH; item: QListWidgetItemH): Boolean; cdecl; external QtIntf name 'QListWidget_isItemHidden';
procedure QListWidget_setItemHidden(handle: QListWidgetH; item: QListWidgetItemH; hide: Boolean); cdecl; external QtIntf name 'QListWidget_setItemHidden';
procedure QListWidget_dropEvent(handle: QListWidgetH; event: QDropEventH); cdecl; external QtIntf name 'QListWidget_dropEvent';
procedure QListWidget_scrollToItem(handle: QListWidgetH; item: QListWidgetItemH; hint: QAbstractItemViewScrollHint); cdecl; external QtIntf name 'QListWidget_scrollToItem';
procedure QListWidget_clear(handle: QListWidgetH); cdecl; external QtIntf name 'QListWidget_clear';


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


function QTreeView_create(parent: QWidgetH = nil): QTreeViewH; cdecl; external QtIntf name 'QTreeView_create';
procedure QTreeView_destroy(handle: QTreeViewH); cdecl; external QtIntf name 'QTreeView_destroy'; 
procedure QTreeView_setModel(handle: QTreeViewH; model: QAbstractItemModelH); cdecl; external QtIntf name 'QTreeView_setModel';
procedure QTreeView_setRootIndex(handle: QTreeViewH; index: QModelIndexH); cdecl; external QtIntf name 'QTreeView_setRootIndex';
procedure QTreeView_setSelectionModel(handle: QTreeViewH; selectionModel: QItemSelectionModelH); cdecl; external QtIntf name 'QTreeView_setSelectionModel';
function QTreeView_header(handle: QTreeViewH): QHeaderViewH; cdecl; external QtIntf name 'QTreeView_header';
procedure QTreeView_setHeader(handle: QTreeViewH; header: QHeaderViewH); cdecl; external QtIntf name 'QTreeView_setHeader';
function QTreeView_indentation(handle: QTreeViewH): Integer; cdecl; external QtIntf name 'QTreeView_indentation';
procedure QTreeView_setIndentation(handle: QTreeViewH; i: Integer); cdecl; external QtIntf name 'QTreeView_setIndentation';
function QTreeView_rootIsDecorated(handle: QTreeViewH): Boolean; cdecl; external QtIntf name 'QTreeView_rootIsDecorated';
procedure QTreeView_setRootIsDecorated(handle: QTreeViewH; show: Boolean); cdecl; external QtIntf name 'QTreeView_setRootIsDecorated';
function QTreeView_uniformRowHeights(handle: QTreeViewH): Boolean; cdecl; external QtIntf name 'QTreeView_uniformRowHeights';
procedure QTreeView_setUniformRowHeights(handle: QTreeViewH; uniform: Boolean); cdecl; external QtIntf name 'QTreeView_setUniformRowHeights';
function QTreeView_itemsExpandable(handle: QTreeViewH): Boolean; cdecl; external QtIntf name 'QTreeView_itemsExpandable';
procedure QTreeView_setItemsExpandable(handle: QTreeViewH; enable: Boolean); cdecl; external QtIntf name 'QTreeView_setItemsExpandable';
function QTreeView_columnViewportPosition(handle: QTreeViewH; column: Integer): Integer; cdecl; external QtIntf name 'QTreeView_columnViewportPosition';
function QTreeView_columnWidth(handle: QTreeViewH; column: Integer): Integer; cdecl; external QtIntf name 'QTreeView_columnWidth';
procedure QTreeView_setColumnWidth(handle: QTreeViewH; column: Integer; width: Integer); cdecl; external QtIntf name 'QTreeView_setColumnWidth';
function QTreeView_columnAt(handle: QTreeViewH; x: Integer): Integer; cdecl; external QtIntf name 'QTreeView_columnAt';
function QTreeView_isColumnHidden(handle: QTreeViewH; column: Integer): Boolean; cdecl; external QtIntf name 'QTreeView_isColumnHidden';
procedure QTreeView_setColumnHidden(handle: QTreeViewH; column: Integer; hide: Boolean); cdecl; external QtIntf name 'QTreeView_setColumnHidden';
function QTreeView_isRowHidden(handle: QTreeViewH; row: Integer; parent: QModelIndexH): Boolean; cdecl; external QtIntf name 'QTreeView_isRowHidden';
procedure QTreeView_setRowHidden(handle: QTreeViewH; row: Integer; parent: QModelIndexH; hide: Boolean); cdecl; external QtIntf name 'QTreeView_setRowHidden';
function QTreeView_isExpanded(handle: QTreeViewH; index: QModelIndexH): Boolean; cdecl; external QtIntf name 'QTreeView_isExpanded';
procedure QTreeView_setExpanded(handle: QTreeViewH; index: QModelIndexH; expand: Boolean); cdecl; external QtIntf name 'QTreeView_setExpanded';
procedure QTreeView_setSortingEnabled(handle: QTreeViewH; enable: Boolean); cdecl; external QtIntf name 'QTreeView_setSortingEnabled';
function QTreeView_isSortingEnabled(handle: QTreeViewH): Boolean; cdecl; external QtIntf name 'QTreeView_isSortingEnabled';
procedure QTreeView_setAnimated(handle: QTreeViewH; enable: Boolean); cdecl; external QtIntf name 'QTreeView_setAnimated';
function QTreeView_isAnimated(handle: QTreeViewH): Boolean; cdecl; external QtIntf name 'QTreeView_isAnimated';
procedure QTreeView_setAllColumnsShowFocus(handle: QTreeViewH; enable: Boolean); cdecl; external QtIntf name 'QTreeView_setAllColumnsShowFocus';
function QTreeView_allColumnsShowFocus(handle: QTreeViewH): Boolean; cdecl; external QtIntf name 'QTreeView_allColumnsShowFocus';
procedure QTreeView_keyboardSearch(handle: QTreeViewH; search: PWideString); cdecl; external QtIntf name 'QTreeView_keyboardSearch';
procedure QTreeView_visualRect(handle: QTreeViewH; retval: PRect; index: QModelIndexH); cdecl; external QtIntf name 'QTreeView_visualRect';
procedure QTreeView_scrollTo(handle: QTreeViewH; index: QModelIndexH; hint: QAbstractItemViewScrollHint); cdecl; external QtIntf name 'QTreeView_scrollTo';
procedure QTreeView_indexAt(handle: QTreeViewH; retval: QModelIndexH; p: PQtPoint); cdecl; external QtIntf name 'QTreeView_indexAt';
procedure QTreeView_indexAbove(handle: QTreeViewH; retval: QModelIndexH; index: QModelIndexH); cdecl; external QtIntf name 'QTreeView_indexAbove';
procedure QTreeView_indexBelow(handle: QTreeViewH; retval: QModelIndexH; index: QModelIndexH); cdecl; external QtIntf name 'QTreeView_indexBelow';
procedure QTreeView_doItemsLayout(handle: QTreeViewH); cdecl; external QtIntf name 'QTreeView_doItemsLayout';
procedure QTreeView_reset(handle: QTreeViewH); cdecl; external QtIntf name 'QTreeView_reset';
procedure QTreeView_sortByColumn(handle: QTreeViewH; column: Integer; order: QtSortOrder); overload; cdecl; external QtIntf name 'QTreeView_sortByColumn';
procedure QTreeView_dataChanged(handle: QTreeViewH; topLeft: QModelIndexH; bottomRight: QModelIndexH); cdecl; external QtIntf name 'QTreeView_dataChanged';
procedure QTreeView_hideColumn(handle: QTreeViewH; column: Integer); cdecl; external QtIntf name 'QTreeView_hideColumn';
procedure QTreeView_showColumn(handle: QTreeViewH; column: Integer); cdecl; external QtIntf name 'QTreeView_showColumn';
procedure QTreeView_expand(handle: QTreeViewH; index: QModelIndexH); cdecl; external QtIntf name 'QTreeView_expand';
procedure QTreeView_collapse(handle: QTreeViewH; index: QModelIndexH); cdecl; external QtIntf name 'QTreeView_collapse';
procedure QTreeView_resizeColumnToContents(handle: QTreeViewH; column: Integer); cdecl; external QtIntf name 'QTreeView_resizeColumnToContents';
procedure QTreeView_sortByColumn(handle: QTreeViewH; column: Integer); overload; cdecl; external QtIntf name 'QTreeView_sortByColumn2';
procedure QTreeView_selectAll(handle: QTreeViewH); cdecl; external QtIntf name 'QTreeView_selectAll';
procedure QTreeView_expandAll(handle: QTreeViewH); cdecl; external QtIntf name 'QTreeView_expandAll';
procedure QTreeView_collapseAll(handle: QTreeViewH); cdecl; external QtIntf name 'QTreeView_collapseAll';


type
  QTreeView_expanded_Event = procedure (index: QModelIndexH) of object cdecl;
  QTreeView_collapsed_Event = procedure (index: QModelIndexH) of object cdecl;



type
  QTreeWidgetItemItemType = (  //QTreeWidgetItem::ItemType (2s)
    QTreeWidgetItemType = 0,
    QTreeWidgetItemUserType = 1000 );

function QTreeWidgetItem_create(_type: Integer = QTreeWidgetItemType): QTreeWidgetItemH; overload; cdecl; external QtIntf name 'QTreeWidgetItem_create';
procedure QTreeWidgetItem_destroy(handle: QTreeWidgetItemH); cdecl; external QtIntf name 'QTreeWidgetItem_destroy'; 
function QTreeWidgetItem_create(strings: QStringListH; _type: Integer = QTreeWidgetItemType): QTreeWidgetItemH; overload; cdecl; external QtIntf name 'QTreeWidgetItem_create2';
function QTreeWidgetItem_create(view: QTreeWidgetH; _type: Integer = QTreeWidgetItemType): QTreeWidgetItemH; overload; cdecl; external QtIntf name 'QTreeWidgetItem_create3';
function QTreeWidgetItem_create(view: QTreeWidgetH; strings: QStringListH; _type: Integer = QTreeWidgetItemType): QTreeWidgetItemH; overload; cdecl; external QtIntf name 'QTreeWidgetItem_create4';
function QTreeWidgetItem_create(view: QTreeWidgetH; after: QTreeWidgetItemH; _type: Integer = QTreeWidgetItemType): QTreeWidgetItemH; overload; cdecl; external QtIntf name 'QTreeWidgetItem_create5';
function QTreeWidgetItem_create(parent: QTreeWidgetItemH; _type: Integer = QTreeWidgetItemType): QTreeWidgetItemH; overload; cdecl; external QtIntf name 'QTreeWidgetItem_create6';
function QTreeWidgetItem_create(parent: QTreeWidgetItemH; strings: QStringListH; _type: Integer = QTreeWidgetItemType): QTreeWidgetItemH; overload; cdecl; external QtIntf name 'QTreeWidgetItem_create7';
function QTreeWidgetItem_create(parent: QTreeWidgetItemH; after: QTreeWidgetItemH; _type: Integer = QTreeWidgetItemType): QTreeWidgetItemH; overload; cdecl; external QtIntf name 'QTreeWidgetItem_create8';
function QTreeWidgetItem_create(other: QTreeWidgetItemH): QTreeWidgetItemH; overload; cdecl; external QtIntf name 'QTreeWidgetItem_create9';
function QTreeWidgetItem_clone(handle: QTreeWidgetItemH): QTreeWidgetItemH; cdecl; external QtIntf name 'QTreeWidgetItem_clone';
function QTreeWidgetItem_treeWidget(handle: QTreeWidgetItemH): QTreeWidgetH; cdecl; external QtIntf name 'QTreeWidgetItem_treeWidget';
procedure QTreeWidgetItem_setSelected(handle: QTreeWidgetItemH; select: Boolean); cdecl; external QtIntf name 'QTreeWidgetItem_setSelected';
function QTreeWidgetItem_isSelected(handle: QTreeWidgetItemH): Boolean; cdecl; external QtIntf name 'QTreeWidgetItem_isSelected';
procedure QTreeWidgetItem_setHidden(handle: QTreeWidgetItemH; hide: Boolean); cdecl; external QtIntf name 'QTreeWidgetItem_setHidden';
function QTreeWidgetItem_isHidden(handle: QTreeWidgetItemH): Boolean; cdecl; external QtIntf name 'QTreeWidgetItem_isHidden';
procedure QTreeWidgetItem_setExpanded(handle: QTreeWidgetItemH; expand: Boolean); cdecl; external QtIntf name 'QTreeWidgetItem_setExpanded';
function QTreeWidgetItem_isExpanded(handle: QTreeWidgetItemH): Boolean; cdecl; external QtIntf name 'QTreeWidgetItem_isExpanded';
function QTreeWidgetItem_flags(handle: QTreeWidgetItemH): QtItemFlags; cdecl; external QtIntf name 'QTreeWidgetItem_flags';
procedure QTreeWidgetItem_setFlags(handle: QTreeWidgetItemH; flags: QtItemFlags); cdecl; external QtIntf name 'QTreeWidgetItem_setFlags';
procedure QTreeWidgetItem_text(handle: QTreeWidgetItemH; retval: PWideString; column: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_text';
procedure QTreeWidgetItem_setText(handle: QTreeWidgetItemH; column: Integer; text: PWideString); cdecl; external QtIntf name 'QTreeWidgetItem_setText';
procedure QTreeWidgetItem_icon(handle: QTreeWidgetItemH; retval: QIconH; column: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_icon';
procedure QTreeWidgetItem_setIcon(handle: QTreeWidgetItemH; column: Integer; icon: QIconH); cdecl; external QtIntf name 'QTreeWidgetItem_setIcon';
procedure QTreeWidgetItem_statusTip(handle: QTreeWidgetItemH; retval: PWideString; column: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_statusTip';
procedure QTreeWidgetItem_setStatusTip(handle: QTreeWidgetItemH; column: Integer; statusTip: PWideString); cdecl; external QtIntf name 'QTreeWidgetItem_setStatusTip';
procedure QTreeWidgetItem_toolTip(handle: QTreeWidgetItemH; retval: PWideString; column: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_toolTip';
procedure QTreeWidgetItem_setToolTip(handle: QTreeWidgetItemH; column: Integer; toolTip: PWideString); cdecl; external QtIntf name 'QTreeWidgetItem_setToolTip';
procedure QTreeWidgetItem_whatsThis(handle: QTreeWidgetItemH; retval: PWideString; column: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_whatsThis';
procedure QTreeWidgetItem_setWhatsThis(handle: QTreeWidgetItemH; column: Integer; whatsThis: PWideString); cdecl; external QtIntf name 'QTreeWidgetItem_setWhatsThis';
procedure QTreeWidgetItem_font(handle: QTreeWidgetItemH; retval: QFontH; column: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_font';
procedure QTreeWidgetItem_setFont(handle: QTreeWidgetItemH; column: Integer; font: QFontH); cdecl; external QtIntf name 'QTreeWidgetItem_setFont';
function QTreeWidgetItem_textAlignment(handle: QTreeWidgetItemH; column: Integer): Integer; cdecl; external QtIntf name 'QTreeWidgetItem_textAlignment';
procedure QTreeWidgetItem_setTextAlignment(handle: QTreeWidgetItemH; column: Integer; alignment: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_setTextAlignment';
procedure QTreeWidgetItem_backgroundColor(handle: QTreeWidgetItemH; retval: PQColor; column: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_backgroundColor';
procedure QTreeWidgetItem_setBackgroundColor(handle: QTreeWidgetItemH; column: Integer; color: PQColor); cdecl; external QtIntf name 'QTreeWidgetItem_setBackgroundColor';
procedure QTreeWidgetItem_background(handle: QTreeWidgetItemH; retval: QBrushH; column: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_background';
procedure QTreeWidgetItem_setBackground(handle: QTreeWidgetItemH; column: Integer; brush: QBrushH); cdecl; external QtIntf name 'QTreeWidgetItem_setBackground';
procedure QTreeWidgetItem_textColor(handle: QTreeWidgetItemH; retval: PQColor; column: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_textColor';
procedure QTreeWidgetItem_setTextColor(handle: QTreeWidgetItemH; column: Integer; color: PQColor); cdecl; external QtIntf name 'QTreeWidgetItem_setTextColor';
procedure QTreeWidgetItem_foreground(handle: QTreeWidgetItemH; retval: QBrushH; column: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_foreground';
procedure QTreeWidgetItem_setForeground(handle: QTreeWidgetItemH; column: Integer; brush: QBrushH); cdecl; external QtIntf name 'QTreeWidgetItem_setForeground';
function QTreeWidgetItem_checkState(handle: QTreeWidgetItemH; column: Integer): QtCheckState; cdecl; external QtIntf name 'QTreeWidgetItem_checkState';
procedure QTreeWidgetItem_setCheckState(handle: QTreeWidgetItemH; column: Integer; state: QtCheckState); cdecl; external QtIntf name 'QTreeWidgetItem_setCheckState';
procedure QTreeWidgetItem_sizeHint(handle: QTreeWidgetItemH; retval: PSize; column: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_sizeHint';
procedure QTreeWidgetItem_setSizeHint(handle: QTreeWidgetItemH; column: Integer; size: PSize); cdecl; external QtIntf name 'QTreeWidgetItem_setSizeHint';
procedure QTreeWidgetItem_data(handle: QTreeWidgetItemH; retval: QVariantH; column: Integer; role: Integer); cdecl; external QtIntf name 'QTreeWidgetItem_data';
procedure QTreeWidgetItem_setData(handle: QTreeWidgetItemH; column: Integer; role: Integer; value: QVariantH); cdecl; external QtIntf name 'QTreeWidgetItem_setData';
procedure QTreeWidgetItem_read(handle: QTreeWidgetItemH; _in: QDataStreamH); cdecl; external QtIntf name 'QTreeWidgetItem_read';
procedure QTreeWidgetItem_write(handle: QTreeWidgetItemH; _out: QDataStreamH); cdecl; external QtIntf name 'QTreeWidgetItem_write';
function QTreeWidgetItem_parent(handle: QTreeWidgetItemH): QTreeWidgetItemH; cdecl; external QtIntf name 'QTreeWidgetItem_parent';
function QTreeWidgetItem_child(handle: QTreeWidgetItemH; index: Integer): QTreeWidgetItemH; cdecl; external QtIntf name 'QTreeWidgetItem_child';
function QTreeWidgetItem_childCount(handle: QTreeWidgetItemH): Integer; cdecl; external QtIntf name 'QTreeWidgetItem_childCount';
function QTreeWidgetItem_columnCount(handle: QTreeWidgetItemH): Integer; cdecl; external QtIntf name 'QTreeWidgetItem_columnCount';
function QTreeWidgetItem_indexOfChild(handle: QTreeWidgetItemH; child: QTreeWidgetItemH): Integer; cdecl; external QtIntf name 'QTreeWidgetItem_indexOfChild';
procedure QTreeWidgetItem_addChild(handle: QTreeWidgetItemH; child: QTreeWidgetItemH); cdecl; external QtIntf name 'QTreeWidgetItem_addChild';
procedure QTreeWidgetItem_insertChild(handle: QTreeWidgetItemH; index: Integer; child: QTreeWidgetItemH); cdecl; external QtIntf name 'QTreeWidgetItem_insertChild';
function QTreeWidgetItem_takeChild(handle: QTreeWidgetItemH; index: Integer): QTreeWidgetItemH; cdecl; external QtIntf name 'QTreeWidgetItem_takeChild';
procedure QTreeWidgetItem_addChildren(handle: QTreeWidgetItemH; children: PIntArray); cdecl; external QtIntf name 'QTreeWidgetItem_addChildren';
procedure QTreeWidgetItem_insertChildren(handle: QTreeWidgetItemH; index: Integer; children: PIntArray); cdecl; external QtIntf name 'QTreeWidgetItem_insertChildren';
procedure QTreeWidgetItem_takeChildren(handle: QTreeWidgetItemH; retval: PIntArray); cdecl; external QtIntf name 'QTreeWidgetItem_takeChildren';
function QTreeWidgetItem_type(handle: QTreeWidgetItemH): Integer; cdecl; external QtIntf name 'QTreeWidgetItem_type';
procedure QTreeWidgetItem_sortChildren(handle: QTreeWidgetItemH; column: Integer; order: QtSortOrder); cdecl; external QtIntf name 'QTreeWidgetItem_sortChildren';

function QTreeWidget_create(parent: QWidgetH = nil): QTreeWidgetH; cdecl; external QtIntf name 'QTreeWidget_create';
procedure QTreeWidget_destroy(handle: QTreeWidgetH); cdecl; external QtIntf name 'QTreeWidget_destroy'; 
function QTreeWidget_columnCount(handle: QTreeWidgetH): Integer; cdecl; external QtIntf name 'QTreeWidget_columnCount';
procedure QTreeWidget_setColumnCount(handle: QTreeWidgetH; columns: Integer); cdecl; external QtIntf name 'QTreeWidget_setColumnCount';
function QTreeWidget_invisibleRootItem(handle: QTreeWidgetH): QTreeWidgetItemH; cdecl; external QtIntf name 'QTreeWidget_invisibleRootItem';
function QTreeWidget_topLevelItem(handle: QTreeWidgetH; index: Integer): QTreeWidgetItemH; cdecl; external QtIntf name 'QTreeWidget_topLevelItem';
function QTreeWidget_topLevelItemCount(handle: QTreeWidgetH): Integer; cdecl; external QtIntf name 'QTreeWidget_topLevelItemCount';
procedure QTreeWidget_insertTopLevelItem(handle: QTreeWidgetH; index: Integer; item: QTreeWidgetItemH); cdecl; external QtIntf name 'QTreeWidget_insertTopLevelItem';
procedure QTreeWidget_addTopLevelItem(handle: QTreeWidgetH; item: QTreeWidgetItemH); cdecl; external QtIntf name 'QTreeWidget_addTopLevelItem';
function QTreeWidget_takeTopLevelItem(handle: QTreeWidgetH; index: Integer): QTreeWidgetItemH; cdecl; external QtIntf name 'QTreeWidget_takeTopLevelItem';
function QTreeWidget_indexOfTopLevelItem(handle: QTreeWidgetH; item: QTreeWidgetItemH): Integer; cdecl; external QtIntf name 'QTreeWidget_indexOfTopLevelItem';
procedure QTreeWidget_insertTopLevelItems(handle: QTreeWidgetH; index: Integer; items: PIntArray); cdecl; external QtIntf name 'QTreeWidget_insertTopLevelItems';
procedure QTreeWidget_addTopLevelItems(handle: QTreeWidgetH; items: PIntArray); cdecl; external QtIntf name 'QTreeWidget_addTopLevelItems';
function QTreeWidget_headerItem(handle: QTreeWidgetH): QTreeWidgetItemH; cdecl; external QtIntf name 'QTreeWidget_headerItem';
procedure QTreeWidget_setHeaderItem(handle: QTreeWidgetH; item: QTreeWidgetItemH); cdecl; external QtIntf name 'QTreeWidget_setHeaderItem';
procedure QTreeWidget_setHeaderLabels(handle: QTreeWidgetH; labels: QStringListH); cdecl; external QtIntf name 'QTreeWidget_setHeaderLabels';
procedure QTreeWidget_setHeaderLabel(handle: QTreeWidgetH; _label: PWideString); cdecl; external QtIntf name 'QTreeWidget_setHeaderLabel';
function QTreeWidget_currentItem(handle: QTreeWidgetH): QTreeWidgetItemH; cdecl; external QtIntf name 'QTreeWidget_currentItem';
function QTreeWidget_currentColumn(handle: QTreeWidgetH): Integer; cdecl; external QtIntf name 'QTreeWidget_currentColumn';
procedure QTreeWidget_setCurrentItem(handle: QTreeWidgetH; item: QTreeWidgetItemH); overload; cdecl; external QtIntf name 'QTreeWidget_setCurrentItem';
procedure QTreeWidget_setCurrentItem(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer); overload; cdecl; external QtIntf name 'QTreeWidget_setCurrentItem2';
function QTreeWidget_itemAt(handle: QTreeWidgetH; p: PQtPoint): QTreeWidgetItemH; overload; cdecl; external QtIntf name 'QTreeWidget_itemAt';
function QTreeWidget_itemAt(handle: QTreeWidgetH; x: Integer; y: Integer): QTreeWidgetItemH; overload; cdecl; external QtIntf name 'QTreeWidget_itemAt2';
procedure QTreeWidget_visualItemRect(handle: QTreeWidgetH; retval: PRect; item: QTreeWidgetItemH); cdecl; external QtIntf name 'QTreeWidget_visualItemRect';
function QTreeWidget_sortColumn(handle: QTreeWidgetH): Integer; cdecl; external QtIntf name 'QTreeWidget_sortColumn';
procedure QTreeWidget_sortItems(handle: QTreeWidgetH; column: Integer; order: QtSortOrder); cdecl; external QtIntf name 'QTreeWidget_sortItems';
procedure QTreeWidget_setSortingEnabled(handle: QTreeWidgetH; enable: Boolean); cdecl; external QtIntf name 'QTreeWidget_setSortingEnabled';
function QTreeWidget_isSortingEnabled(handle: QTreeWidgetH): Boolean; cdecl; external QtIntf name 'QTreeWidget_isSortingEnabled';
procedure QTreeWidget_editItem(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer = 0); cdecl; external QtIntf name 'QTreeWidget_editItem';
procedure QTreeWidget_openPersistentEditor(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer = 0); cdecl; external QtIntf name 'QTreeWidget_openPersistentEditor';
procedure QTreeWidget_closePersistentEditor(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer = 0); cdecl; external QtIntf name 'QTreeWidget_closePersistentEditor';
function QTreeWidget_itemWidget(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer): QWidgetH; cdecl; external QtIntf name 'QTreeWidget_itemWidget';
procedure QTreeWidget_setItemWidget(handle: QTreeWidgetH; item: QTreeWidgetItemH; column: Integer; widget: QWidgetH); cdecl; external QtIntf name 'QTreeWidget_setItemWidget';
function QTreeWidget_isItemSelected(handle: QTreeWidgetH; item: QTreeWidgetItemH): Boolean; cdecl; external QtIntf name 'QTreeWidget_isItemSelected';
procedure QTreeWidget_setItemSelected(handle: QTreeWidgetH; item: QTreeWidgetItemH; select: Boolean); cdecl; external QtIntf name 'QTreeWidget_setItemSelected';
procedure QTreeWidget_selectedItems(handle: QTreeWidgetH; retval: PIntArray); cdecl; external QtIntf name 'QTreeWidget_selectedItems';
procedure QTreeWidget_findItems(handle: QTreeWidgetH; retval: PIntArray; text: PWideString; flags: QtMatchFlags; column: Integer = 0); cdecl; external QtIntf name 'QTreeWidget_findItems';
function QTreeWidget_isItemHidden(handle: QTreeWidgetH; item: QTreeWidgetItemH): Boolean; cdecl; external QtIntf name 'QTreeWidget_isItemHidden';
procedure QTreeWidget_setItemHidden(handle: QTreeWidgetH; item: QTreeWidgetItemH; hide: Boolean); cdecl; external QtIntf name 'QTreeWidget_setItemHidden';
function QTreeWidget_isItemExpanded(handle: QTreeWidgetH; item: QTreeWidgetItemH): Boolean; cdecl; external QtIntf name 'QTreeWidget_isItemExpanded';
procedure QTreeWidget_setItemExpanded(handle: QTreeWidgetH; item: QTreeWidgetItemH; expand: Boolean); cdecl; external QtIntf name 'QTreeWidget_setItemExpanded';
procedure QTreeWidget_scrollToItem(handle: QTreeWidgetH; item: QTreeWidgetItemH; hint: QAbstractItemViewScrollHint); cdecl; external QtIntf name 'QTreeWidget_scrollToItem';
procedure QTreeWidget_expandItem(handle: QTreeWidgetH; item: QTreeWidgetItemH); cdecl; external QtIntf name 'QTreeWidget_expandItem';
procedure QTreeWidget_collapseItem(handle: QTreeWidgetH; item: QTreeWidgetItemH); cdecl; external QtIntf name 'QTreeWidget_collapseItem';
procedure QTreeWidget_clear(handle: QTreeWidgetH); cdecl; external QtIntf name 'QTreeWidget_clear';


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
  QHeaderViewResizeMode = cardinal; //  QHeaderView::ResizeMode (4)

const
    QHeaderViewInteractive = 0 { $0 };
    QHeaderViewStretch = 1 { $1 };
    QHeaderViewFixed = 2 { $2 };
    QHeaderViewResizeToContents = 3 { $3 };
    QHeaderViewCustom = 2 { $2 };


function QHeaderView_create(orientation: QtOrientation; parent: QWidgetH = nil): QHeaderViewH; cdecl; external QtIntf name 'QHeaderView_create';
procedure QHeaderView_destroy(handle: QHeaderViewH); cdecl; external QtIntf name 'QHeaderView_destroy'; 
procedure QHeaderView_setModel(handle: QHeaderViewH; model: QAbstractItemModelH); cdecl; external QtIntf name 'QHeaderView_setModel';
function QHeaderView_orientation(handle: QHeaderViewH): QtOrientation; cdecl; external QtIntf name 'QHeaderView_orientation';
function QHeaderView_offset(handle: QHeaderViewH): Integer; cdecl; external QtIntf name 'QHeaderView_offset';
function QHeaderView_length(handle: QHeaderViewH): Integer; cdecl; external QtIntf name 'QHeaderView_length';
procedure QHeaderView_sizeHint(handle: QHeaderViewH; retval: PSize); cdecl; external QtIntf name 'QHeaderView_sizeHint';
function QHeaderView_sectionSizeHint(handle: QHeaderViewH; logicalIndex: Integer): Integer; cdecl; external QtIntf name 'QHeaderView_sectionSizeHint';
function QHeaderView_visualIndexAt(handle: QHeaderViewH; position: Integer): Integer; cdecl; external QtIntf name 'QHeaderView_visualIndexAt';
function QHeaderView_logicalIndexAt(handle: QHeaderViewH; position: Integer): Integer; overload; cdecl; external QtIntf name 'QHeaderView_logicalIndexAt';
function QHeaderView_logicalIndexAt(handle: QHeaderViewH; x: Integer; y: Integer): Integer; overload; cdecl; external QtIntf name 'QHeaderView_logicalIndexAt2';
function QHeaderView_logicalIndexAt(handle: QHeaderViewH; pos: PQtPoint): Integer; overload; cdecl; external QtIntf name 'QHeaderView_logicalIndexAt3';
function QHeaderView_sectionSize(handle: QHeaderViewH; logicalIndex: Integer): Integer; cdecl; external QtIntf name 'QHeaderView_sectionSize';
function QHeaderView_sectionPosition(handle: QHeaderViewH; logicalIndex: Integer): Integer; cdecl; external QtIntf name 'QHeaderView_sectionPosition';
function QHeaderView_sectionViewportPosition(handle: QHeaderViewH; logicalIndex: Integer): Integer; cdecl; external QtIntf name 'QHeaderView_sectionViewportPosition';
procedure QHeaderView_moveSection(handle: QHeaderViewH; from: Integer; _to: Integer); cdecl; external QtIntf name 'QHeaderView_moveSection';
procedure QHeaderView_swapSections(handle: QHeaderViewH; first: Integer; second: Integer); cdecl; external QtIntf name 'QHeaderView_swapSections';
procedure QHeaderView_resizeSection(handle: QHeaderViewH; logicalIndex: Integer; size: Integer); cdecl; external QtIntf name 'QHeaderView_resizeSection';
procedure QHeaderView_resizeSections(handle: QHeaderViewH; mode: QHeaderViewResizeMode); cdecl; external QtIntf name 'QHeaderView_resizeSections';
function QHeaderView_isSectionHidden(handle: QHeaderViewH; logicalIndex: Integer): Boolean; cdecl; external QtIntf name 'QHeaderView_isSectionHidden';
procedure QHeaderView_setSectionHidden(handle: QHeaderViewH; logicalIndex: Integer; hide: Boolean); cdecl; external QtIntf name 'QHeaderView_setSectionHidden';
function QHeaderView_hiddenSectionCount(handle: QHeaderViewH): Integer; cdecl; external QtIntf name 'QHeaderView_hiddenSectionCount';
procedure QHeaderView_hideSection(handle: QHeaderViewH; logicalIndex: Integer); cdecl; external QtIntf name 'QHeaderView_hideSection';
procedure QHeaderView_showSection(handle: QHeaderViewH; logicalIndex: Integer); cdecl; external QtIntf name 'QHeaderView_showSection';
function QHeaderView_count(handle: QHeaderViewH): Integer; cdecl; external QtIntf name 'QHeaderView_count';
function QHeaderView_visualIndex(handle: QHeaderViewH; logicalIndex: Integer): Integer; cdecl; external QtIntf name 'QHeaderView_visualIndex';
function QHeaderView_logicalIndex(handle: QHeaderViewH; visualIndex: Integer): Integer; cdecl; external QtIntf name 'QHeaderView_logicalIndex';
procedure QHeaderView_setMovable(handle: QHeaderViewH; movable: Boolean); cdecl; external QtIntf name 'QHeaderView_setMovable';
function QHeaderView_isMovable(handle: QHeaderViewH): Boolean; cdecl; external QtIntf name 'QHeaderView_isMovable';
procedure QHeaderView_setClickable(handle: QHeaderViewH; clickable: Boolean); cdecl; external QtIntf name 'QHeaderView_setClickable';
function QHeaderView_isClickable(handle: QHeaderViewH): Boolean; cdecl; external QtIntf name 'QHeaderView_isClickable';
procedure QHeaderView_setHighlightSections(handle: QHeaderViewH; highlight: Boolean); cdecl; external QtIntf name 'QHeaderView_setHighlightSections';
function QHeaderView_highlightSections(handle: QHeaderViewH): Boolean; cdecl; external QtIntf name 'QHeaderView_highlightSections';
procedure QHeaderView_setResizeMode(handle: QHeaderViewH; mode: QHeaderViewResizeMode); overload; cdecl; external QtIntf name 'QHeaderView_setResizeMode';
procedure QHeaderView_setResizeMode(handle: QHeaderViewH; logicalIndex: Integer; mode: QHeaderViewResizeMode); overload; cdecl; external QtIntf name 'QHeaderView_setResizeMode2';
function QHeaderView_resizeMode(handle: QHeaderViewH; logicalIndex: Integer): QHeaderViewResizeMode; cdecl; external QtIntf name 'QHeaderView_resizeMode';
function QHeaderView_stretchSectionCount(handle: QHeaderViewH): Integer; cdecl; external QtIntf name 'QHeaderView_stretchSectionCount';
procedure QHeaderView_setSortIndicatorShown(handle: QHeaderViewH; show: Boolean); cdecl; external QtIntf name 'QHeaderView_setSortIndicatorShown';
function QHeaderView_isSortIndicatorShown(handle: QHeaderViewH): Boolean; cdecl; external QtIntf name 'QHeaderView_isSortIndicatorShown';
procedure QHeaderView_setSortIndicator(handle: QHeaderViewH; logicalIndex: Integer; order: QtSortOrder); cdecl; external QtIntf name 'QHeaderView_setSortIndicator';
function QHeaderView_sortIndicatorSection(handle: QHeaderViewH): Integer; cdecl; external QtIntf name 'QHeaderView_sortIndicatorSection';
function QHeaderView_sortIndicatorOrder(handle: QHeaderViewH): QtSortOrder; cdecl; external QtIntf name 'QHeaderView_sortIndicatorOrder';
function QHeaderView_stretchLastSection(handle: QHeaderViewH): Boolean; cdecl; external QtIntf name 'QHeaderView_stretchLastSection';
procedure QHeaderView_setStretchLastSection(handle: QHeaderViewH; stretch: Boolean); cdecl; external QtIntf name 'QHeaderView_setStretchLastSection';
function QHeaderView_cascadingSectionResizes(handle: QHeaderViewH): Boolean; cdecl; external QtIntf name 'QHeaderView_cascadingSectionResizes';
procedure QHeaderView_setCascadingSectionResizes(handle: QHeaderViewH; enable: Boolean); cdecl; external QtIntf name 'QHeaderView_setCascadingSectionResizes';
function QHeaderView_defaultSectionSize(handle: QHeaderViewH): Integer; cdecl; external QtIntf name 'QHeaderView_defaultSectionSize';
procedure QHeaderView_setDefaultSectionSize(handle: QHeaderViewH; size: Integer); cdecl; external QtIntf name 'QHeaderView_setDefaultSectionSize';
function QHeaderView_minimumSectionSize(handle: QHeaderViewH): Integer; cdecl; external QtIntf name 'QHeaderView_minimumSectionSize';
procedure QHeaderView_setMinimumSectionSize(handle: QHeaderViewH; size: Integer); cdecl; external QtIntf name 'QHeaderView_setMinimumSectionSize';
function QHeaderView_defaultAlignment(handle: QHeaderViewH): QtAlignment; cdecl; external QtIntf name 'QHeaderView_defaultAlignment';
procedure QHeaderView_setDefaultAlignment(handle: QHeaderViewH; alignment: QtAlignment); cdecl; external QtIntf name 'QHeaderView_setDefaultAlignment';
procedure QHeaderView_doItemsLayout(handle: QHeaderViewH); cdecl; external QtIntf name 'QHeaderView_doItemsLayout';
function QHeaderView_sectionsMoved(handle: QHeaderViewH): Boolean; cdecl; external QtIntf name 'QHeaderView_sectionsMoved';
function QHeaderView_sectionsHidden(handle: QHeaderViewH): Boolean; cdecl; external QtIntf name 'QHeaderView_sectionsHidden';
procedure QHeaderView_setOffset(handle: QHeaderViewH; offset: Integer); cdecl; external QtIntf name 'QHeaderView_setOffset';
procedure QHeaderView_setOffsetToSectionPosition(handle: QHeaderViewH; visualIndex: Integer); cdecl; external QtIntf name 'QHeaderView_setOffsetToSectionPosition';
procedure QHeaderView_headerDataChanged(handle: QHeaderViewH; orientation: QtOrientation; logicalFirst: Integer; logicalLast: Integer); cdecl; external QtIntf name 'QHeaderView_headerDataChanged';


type
  QHeaderView_sectionMoved_Event = procedure (logicalIndex: Integer; oldVisualIndex: Integer; newVisualIndex: Integer) of object cdecl;
  QHeaderView_sectionResized_Event = procedure (logicalIndex: Integer; oldSize: Integer; newSize: Integer) of object cdecl;
  QHeaderView_sectionPressed_Event = procedure (logicalIndex: Integer) of object cdecl;
  QHeaderView_sectionClicked_Event = procedure (logicalIndex: Integer) of object cdecl;
  QHeaderView_sectionDoubleClicked_Event = procedure (logicalIndex: Integer) of object cdecl;
  QHeaderView_sectionCountChanged_Event = procedure (oldCount: Integer; newCount: Integer) of object cdecl;
  QHeaderView_sectionHandleDoubleClicked_Event = procedure (logicalIndex: Integer) of object cdecl;
  QHeaderView_sectionAutoResize_Event = procedure (logicalIndex: Integer; mode: QHeaderViewResizeMode) of object cdecl;
  QHeaderView_geometriesChanged_Event = procedure () of object cdecl;



type
  QStandardItemItemType = (  //QStandardItem::ItemType (2s)
    QStandardItemType = 0,
    QStandardItemUserType = 1000 );

function QStandardItem_create(): QStandardItemH; overload; cdecl; external QtIntf name 'QStandardItem_create';
procedure QStandardItem_destroy(handle: QStandardItemH); cdecl; external QtIntf name 'QStandardItem_destroy'; 
function QStandardItem_create(text: PWideString): QStandardItemH; overload; cdecl; external QtIntf name 'QStandardItem_create2';
function QStandardItem_create(icon: QIconH; text: PWideString): QStandardItemH; overload; cdecl; external QtIntf name 'QStandardItem_create3';
function QStandardItem_create(rows: Integer; columns: Integer = 1): QStandardItemH; overload; cdecl; external QtIntf name 'QStandardItem_create4';
procedure QStandardItem_data(handle: QStandardItemH; retval: QVariantH; role: QtItemDataRole); cdecl; external QtIntf name 'QStandardItem_data';
procedure QStandardItem_setData(handle: QStandardItemH; value: QVariantH; role: QtItemDataRole); cdecl; external QtIntf name 'QStandardItem_setData';
procedure QStandardItem_text(handle: QStandardItemH; retval: PWideString); cdecl; external QtIntf name 'QStandardItem_text';
procedure QStandardItem_setText(handle: QStandardItemH; text: PWideString); cdecl; external QtIntf name 'QStandardItem_setText';
procedure QStandardItem_icon(handle: QStandardItemH; retval: QIconH); cdecl; external QtIntf name 'QStandardItem_icon';
procedure QStandardItem_setIcon(handle: QStandardItemH; icon: QIconH); cdecl; external QtIntf name 'QStandardItem_setIcon';
procedure QStandardItem_toolTip(handle: QStandardItemH; retval: PWideString); cdecl; external QtIntf name 'QStandardItem_toolTip';
procedure QStandardItem_setToolTip(handle: QStandardItemH; toolTip: PWideString); cdecl; external QtIntf name 'QStandardItem_setToolTip';
procedure QStandardItem_statusTip(handle: QStandardItemH; retval: PWideString); cdecl; external QtIntf name 'QStandardItem_statusTip';
procedure QStandardItem_setStatusTip(handle: QStandardItemH; statusTip: PWideString); cdecl; external QtIntf name 'QStandardItem_setStatusTip';
procedure QStandardItem_whatsThis(handle: QStandardItemH; retval: PWideString); cdecl; external QtIntf name 'QStandardItem_whatsThis';
procedure QStandardItem_setWhatsThis(handle: QStandardItemH; whatsThis: PWideString); cdecl; external QtIntf name 'QStandardItem_setWhatsThis';
procedure QStandardItem_sizeHint(handle: QStandardItemH; retval: PSize); cdecl; external QtIntf name 'QStandardItem_sizeHint';
procedure QStandardItem_setSizeHint(handle: QStandardItemH; sizeHint: PSize); cdecl; external QtIntf name 'QStandardItem_setSizeHint';
procedure QStandardItem_font(handle: QStandardItemH; retval: QFontH); cdecl; external QtIntf name 'QStandardItem_font';
procedure QStandardItem_setFont(handle: QStandardItemH; font: QFontH); cdecl; external QtIntf name 'QStandardItem_setFont';
function QStandardItem_textAlignment(handle: QStandardItemH): QtAlignment; cdecl; external QtIntf name 'QStandardItem_textAlignment';
procedure QStandardItem_setTextAlignment(handle: QStandardItemH; textAlignment: QtAlignment); cdecl; external QtIntf name 'QStandardItem_setTextAlignment';
procedure QStandardItem_background(handle: QStandardItemH; retval: QBrushH); cdecl; external QtIntf name 'QStandardItem_background';
procedure QStandardItem_setBackground(handle: QStandardItemH; brush: QBrushH); cdecl; external QtIntf name 'QStandardItem_setBackground';
procedure QStandardItem_foreground(handle: QStandardItemH; retval: QBrushH); cdecl; external QtIntf name 'QStandardItem_foreground';
procedure QStandardItem_setForeground(handle: QStandardItemH; brush: QBrushH); cdecl; external QtIntf name 'QStandardItem_setForeground';
function QStandardItem_checkState(handle: QStandardItemH): QtCheckState; cdecl; external QtIntf name 'QStandardItem_checkState';
procedure QStandardItem_setCheckState(handle: QStandardItemH; checkState: QtCheckState); cdecl; external QtIntf name 'QStandardItem_setCheckState';
procedure QStandardItem_accessibleText(handle: QStandardItemH; retval: PWideString); cdecl; external QtIntf name 'QStandardItem_accessibleText';
procedure QStandardItem_setAccessibleText(handle: QStandardItemH; accessibleText: PWideString); cdecl; external QtIntf name 'QStandardItem_setAccessibleText';
procedure QStandardItem_accessibleDescription(handle: QStandardItemH; retval: PWideString); cdecl; external QtIntf name 'QStandardItem_accessibleDescription';
procedure QStandardItem_setAccessibleDescription(handle: QStandardItemH; accessibleDescription: PWideString); cdecl; external QtIntf name 'QStandardItem_setAccessibleDescription';
function QStandardItem_flags(handle: QStandardItemH): QtItemFlags; cdecl; external QtIntf name 'QStandardItem_flags';
procedure QStandardItem_setFlags(handle: QStandardItemH; flags: QtItemFlags); cdecl; external QtIntf name 'QStandardItem_setFlags';
function QStandardItem_isEnabled(handle: QStandardItemH): Boolean; cdecl; external QtIntf name 'QStandardItem_isEnabled';
procedure QStandardItem_setEnabled(handle: QStandardItemH; enabled: Boolean); cdecl; external QtIntf name 'QStandardItem_setEnabled';
function QStandardItem_isEditable(handle: QStandardItemH): Boolean; cdecl; external QtIntf name 'QStandardItem_isEditable';
procedure QStandardItem_setEditable(handle: QStandardItemH; editable: Boolean); cdecl; external QtIntf name 'QStandardItem_setEditable';
function QStandardItem_isSelectable(handle: QStandardItemH): Boolean; cdecl; external QtIntf name 'QStandardItem_isSelectable';
procedure QStandardItem_setSelectable(handle: QStandardItemH; selectable: Boolean); cdecl; external QtIntf name 'QStandardItem_setSelectable';
function QStandardItem_isCheckable(handle: QStandardItemH): Boolean; cdecl; external QtIntf name 'QStandardItem_isCheckable';
procedure QStandardItem_setCheckable(handle: QStandardItemH; checkable: Boolean); cdecl; external QtIntf name 'QStandardItem_setCheckable';
function QStandardItem_isTristate(handle: QStandardItemH): Boolean; cdecl; external QtIntf name 'QStandardItem_isTristate';
procedure QStandardItem_setTristate(handle: QStandardItemH; tristate: Boolean); cdecl; external QtIntf name 'QStandardItem_setTristate';
function QStandardItem_isDragEnabled(handle: QStandardItemH): Boolean; cdecl; external QtIntf name 'QStandardItem_isDragEnabled';
procedure QStandardItem_setDragEnabled(handle: QStandardItemH; dragEnabled: Boolean); cdecl; external QtIntf name 'QStandardItem_setDragEnabled';
function QStandardItem_isDropEnabled(handle: QStandardItemH): Boolean; cdecl; external QtIntf name 'QStandardItem_isDropEnabled';
procedure QStandardItem_setDropEnabled(handle: QStandardItemH; dropEnabled: Boolean); cdecl; external QtIntf name 'QStandardItem_setDropEnabled';
function QStandardItem_parent(handle: QStandardItemH): QStandardItemH; cdecl; external QtIntf name 'QStandardItem_parent';
function QStandardItem_row(handle: QStandardItemH): Integer; cdecl; external QtIntf name 'QStandardItem_row';
function QStandardItem_column(handle: QStandardItemH): Integer; cdecl; external QtIntf name 'QStandardItem_column';
procedure QStandardItem_index(handle: QStandardItemH; retval: QModelIndexH); cdecl; external QtIntf name 'QStandardItem_index';
function QStandardItem_model(handle: QStandardItemH): QStandardItemModelH; cdecl; external QtIntf name 'QStandardItem_model';
function QStandardItem_rowCount(handle: QStandardItemH): Integer; cdecl; external QtIntf name 'QStandardItem_rowCount';
procedure QStandardItem_setRowCount(handle: QStandardItemH; rows: Integer); cdecl; external QtIntf name 'QStandardItem_setRowCount';
function QStandardItem_columnCount(handle: QStandardItemH): Integer; cdecl; external QtIntf name 'QStandardItem_columnCount';
procedure QStandardItem_setColumnCount(handle: QStandardItemH; columns: Integer); cdecl; external QtIntf name 'QStandardItem_setColumnCount';
function QStandardItem_hasChildren(handle: QStandardItemH): Boolean; cdecl; external QtIntf name 'QStandardItem_hasChildren';
function QStandardItem_child(handle: QStandardItemH; row: Integer; column: Integer = 0): QStandardItemH; cdecl; external QtIntf name 'QStandardItem_child';
procedure QStandardItem_setChild(handle: QStandardItemH; row: Integer; column: Integer; item: QStandardItemH); overload; cdecl; external QtIntf name 'QStandardItem_setChild';
procedure QStandardItem_setChild(handle: QStandardItemH; row: Integer; item: QStandardItemH); overload; cdecl; external QtIntf name 'QStandardItem_setChild2';
procedure QStandardItem_insertRow(handle: QStandardItemH; row: Integer; items: PIntArray); overload; cdecl; external QtIntf name 'QStandardItem_insertRow';
procedure QStandardItem_insertColumn(handle: QStandardItemH; column: Integer; items: PIntArray); cdecl; external QtIntf name 'QStandardItem_insertColumn';
procedure QStandardItem_insertRows(handle: QStandardItemH; row: Integer; count: Integer); cdecl; external QtIntf name 'QStandardItem_insertRows';
procedure QStandardItem_insertColumns(handle: QStandardItemH; column: Integer; count: Integer); cdecl; external QtIntf name 'QStandardItem_insertColumns';
procedure QStandardItem_removeRow(handle: QStandardItemH; row: Integer); cdecl; external QtIntf name 'QStandardItem_removeRow';
procedure QStandardItem_removeColumn(handle: QStandardItemH; column: Integer); cdecl; external QtIntf name 'QStandardItem_removeColumn';
procedure QStandardItem_removeRows(handle: QStandardItemH; row: Integer; count: Integer); cdecl; external QtIntf name 'QStandardItem_removeRows';
procedure QStandardItem_removeColumns(handle: QStandardItemH; column: Integer; count: Integer); cdecl; external QtIntf name 'QStandardItem_removeColumns';
procedure QStandardItem_appendRow(handle: QStandardItemH; items: PIntArray); overload; cdecl; external QtIntf name 'QStandardItem_appendRow';
procedure QStandardItem_appendColumn(handle: QStandardItemH; items: PIntArray); cdecl; external QtIntf name 'QStandardItem_appendColumn';
procedure QStandardItem_insertRow(handle: QStandardItemH; row: Integer; item: QStandardItemH); overload; cdecl; external QtIntf name 'QStandardItem_insertRow2';
procedure QStandardItem_appendRow(handle: QStandardItemH; item: QStandardItemH); overload; cdecl; external QtIntf name 'QStandardItem_appendRow2';
function QStandardItem_takeChild(handle: QStandardItemH; row: Integer; column: Integer = 0): QStandardItemH; cdecl; external QtIntf name 'QStandardItem_takeChild';
procedure QStandardItem_takeRow(handle: QStandardItemH; retval: PIntArray; row: Integer); cdecl; external QtIntf name 'QStandardItem_takeRow';
procedure QStandardItem_takeColumn(handle: QStandardItemH; retval: PIntArray; column: Integer); cdecl; external QtIntf name 'QStandardItem_takeColumn';
procedure QStandardItem_sortChildren(handle: QStandardItemH; column: Integer; order: QtSortOrder = QtAscendingOrder); cdecl; external QtIntf name 'QStandardItem_sortChildren';
function QStandardItem_clone(handle: QStandardItemH): QStandardItemH; cdecl; external QtIntf name 'QStandardItem_clone';
function QStandardItem_type(handle: QStandardItemH): Integer; cdecl; external QtIntf name 'QStandardItem_type';
procedure QStandardItem_read(handle: QStandardItemH; _in: QDataStreamH); cdecl; external QtIntf name 'QStandardItem_read';
procedure QStandardItem_write(handle: QStandardItemH; _out: QDataStreamH); cdecl; external QtIntf name 'QStandardItem_write';

function QStandardItemModel_create(parent: QObjectH = nil): QStandardItemModelH; overload; cdecl; external QtIntf name 'QStandardItemModel_create';
procedure QStandardItemModel_destroy(handle: QStandardItemModelH); cdecl; external QtIntf name 'QStandardItemModel_destroy'; 
function QStandardItemModel_create(rows: Integer; columns: Integer; parent: QObjectH = nil): QStandardItemModelH; overload; cdecl; external QtIntf name 'QStandardItemModel_create2';
procedure QStandardItemModel_index(handle: QStandardItemModelH; retval: QModelIndexH; row: Integer; column: Integer; parent: QModelIndexH = nil); cdecl; external QtIntf name 'QStandardItemModel_index';
procedure QStandardItemModel_parent(handle: QStandardItemModelH; retval: QModelIndexH; child: QModelIndexH); cdecl; external QtIntf name 'QStandardItemModel_parent';
function QStandardItemModel_rowCount(handle: QStandardItemModelH; parent: QModelIndexH = nil): Integer; cdecl; external QtIntf name 'QStandardItemModel_rowCount';
function QStandardItemModel_columnCount(handle: QStandardItemModelH; parent: QModelIndexH = nil): Integer; cdecl; external QtIntf name 'QStandardItemModel_columnCount';
function QStandardItemModel_hasChildren(handle: QStandardItemModelH; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QStandardItemModel_hasChildren';
procedure QStandardItemModel_data(handle: QStandardItemModelH; retval: QVariantH; index: QModelIndexH; role: QtItemDataRole = QtDisplayRole); cdecl; external QtIntf name 'QStandardItemModel_data';
function QStandardItemModel_setData(handle: QStandardItemModelH; index: QModelIndexH; value: QVariantH; role: QtItemDataRole = QtEditRole): Boolean; cdecl; external QtIntf name 'QStandardItemModel_setData';
procedure QStandardItemModel_headerData(handle: QStandardItemModelH; retval: QVariantH; section: Integer; orientation: QtOrientation; role: QtItemDataRole = QtDisplayRole); cdecl; external QtIntf name 'QStandardItemModel_headerData';
function QStandardItemModel_setHeaderData(handle: QStandardItemModelH; section: Integer; orientation: QtOrientation; value: QVariantH; role: QtItemDataRole = QtEditRole): Boolean; cdecl; external QtIntf name 'QStandardItemModel_setHeaderData';
function QStandardItemModel_insertRows(handle: QStandardItemModelH; row: Integer; count: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QStandardItemModel_insertRows';
function QStandardItemModel_insertColumns(handle: QStandardItemModelH; column: Integer; count: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QStandardItemModel_insertColumns';
function QStandardItemModel_removeRows(handle: QStandardItemModelH; row: Integer; count: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QStandardItemModel_removeRows';
function QStandardItemModel_removeColumns(handle: QStandardItemModelH; column: Integer; count: Integer; parent: QModelIndexH = nil): Boolean; cdecl; external QtIntf name 'QStandardItemModel_removeColumns';
function QStandardItemModel_flags(handle: QStandardItemModelH; index: QModelIndexH): QtItemFlags; cdecl; external QtIntf name 'QStandardItemModel_flags';
function QStandardItemModel_supportedDropActions(handle: QStandardItemModelH): QtDropActions; cdecl; external QtIntf name 'QStandardItemModel_supportedDropActions';
procedure QStandardItemModel_clear(handle: QStandardItemModelH); cdecl; external QtIntf name 'QStandardItemModel_clear';
procedure QStandardItemModel_sort(handle: QStandardItemModelH; column: Integer; order: QtSortOrder = QtAscendingOrder); cdecl; external QtIntf name 'QStandardItemModel_sort';
function QStandardItemModel_itemFromIndex(handle: QStandardItemModelH; index: QModelIndexH): QStandardItemH; cdecl; external QtIntf name 'QStandardItemModel_itemFromIndex';
procedure QStandardItemModel_indexFromItem(handle: QStandardItemModelH; retval: QModelIndexH; item: QStandardItemH); cdecl; external QtIntf name 'QStandardItemModel_indexFromItem';
function QStandardItemModel_item(handle: QStandardItemModelH; row: Integer; column: Integer = 0): QStandardItemH; cdecl; external QtIntf name 'QStandardItemModel_item';
procedure QStandardItemModel_setItem(handle: QStandardItemModelH; row: Integer; column: Integer; item: QStandardItemH); overload; cdecl; external QtIntf name 'QStandardItemModel_setItem';
procedure QStandardItemModel_setItem(handle: QStandardItemModelH; row: Integer; item: QStandardItemH); overload; cdecl; external QtIntf name 'QStandardItemModel_setItem2';
function QStandardItemModel_invisibleRootItem(handle: QStandardItemModelH): QStandardItemH; cdecl; external QtIntf name 'QStandardItemModel_invisibleRootItem';
function QStandardItemModel_horizontalHeaderItem(handle: QStandardItemModelH; column: Integer): QStandardItemH; cdecl; external QtIntf name 'QStandardItemModel_horizontalHeaderItem';
procedure QStandardItemModel_setHorizontalHeaderItem(handle: QStandardItemModelH; column: Integer; item: QStandardItemH); cdecl; external QtIntf name 'QStandardItemModel_setHorizontalHeaderItem';
function QStandardItemModel_verticalHeaderItem(handle: QStandardItemModelH; row: Integer): QStandardItemH; cdecl; external QtIntf name 'QStandardItemModel_verticalHeaderItem';
procedure QStandardItemModel_setVerticalHeaderItem(handle: QStandardItemModelH; row: Integer; item: QStandardItemH); cdecl; external QtIntf name 'QStandardItemModel_setVerticalHeaderItem';
procedure QStandardItemModel_setHorizontalHeaderLabels(handle: QStandardItemModelH; labels: QStringListH); cdecl; external QtIntf name 'QStandardItemModel_setHorizontalHeaderLabels';
procedure QStandardItemModel_setVerticalHeaderLabels(handle: QStandardItemModelH; labels: QStringListH); cdecl; external QtIntf name 'QStandardItemModel_setVerticalHeaderLabels';
procedure QStandardItemModel_setRowCount(handle: QStandardItemModelH; rows: Integer); cdecl; external QtIntf name 'QStandardItemModel_setRowCount';
procedure QStandardItemModel_setColumnCount(handle: QStandardItemModelH; columns: Integer); cdecl; external QtIntf name 'QStandardItemModel_setColumnCount';
procedure QStandardItemModel_appendRow(handle: QStandardItemModelH; items: PIntArray); overload; cdecl; external QtIntf name 'QStandardItemModel_appendRow';
procedure QStandardItemModel_appendColumn(handle: QStandardItemModelH; items: PIntArray); cdecl; external QtIntf name 'QStandardItemModel_appendColumn';
procedure QStandardItemModel_appendRow(handle: QStandardItemModelH; item: QStandardItemH); overload; cdecl; external QtIntf name 'QStandardItemModel_appendRow2';
procedure QStandardItemModel_insertRow(handle: QStandardItemModelH; row: Integer; items: PIntArray); overload; cdecl; external QtIntf name 'QStandardItemModel_insertRow';
procedure QStandardItemModel_insertColumn(handle: QStandardItemModelH; column: Integer; items: PIntArray); overload; cdecl; external QtIntf name 'QStandardItemModel_insertColumn';
procedure QStandardItemModel_insertRow(handle: QStandardItemModelH; row: Integer; item: QStandardItemH); overload; cdecl; external QtIntf name 'QStandardItemModel_insertRow2';
function QStandardItemModel_insertRow(handle: QStandardItemModelH; row: Integer; parent: QModelIndexH = nil): Boolean; overload; cdecl; external QtIntf name 'QStandardItemModel_insertRow3';
function QStandardItemModel_insertColumn(handle: QStandardItemModelH; column: Integer; parent: QModelIndexH = nil): Boolean; overload; cdecl; external QtIntf name 'QStandardItemModel_insertColumn2';
function QStandardItemModel_takeItem(handle: QStandardItemModelH; row: Integer; column: Integer = 0): QStandardItemH; cdecl; external QtIntf name 'QStandardItemModel_takeItem';
procedure QStandardItemModel_takeRow(handle: QStandardItemModelH; retval: PIntArray; row: Integer); cdecl; external QtIntf name 'QStandardItemModel_takeRow';
procedure QStandardItemModel_takeColumn(handle: QStandardItemModelH; retval: PIntArray; column: Integer); cdecl; external QtIntf name 'QStandardItemModel_takeColumn';
function QStandardItemModel_takeHorizontalHeaderItem(handle: QStandardItemModelH; column: Integer): QStandardItemH; cdecl; external QtIntf name 'QStandardItemModel_takeHorizontalHeaderItem';
function QStandardItemModel_takeVerticalHeaderItem(handle: QStandardItemModelH; row: Integer): QStandardItemH; cdecl; external QtIntf name 'QStandardItemModel_takeVerticalHeaderItem';
function QStandardItemModel_itemPrototype(handle: QStandardItemModelH): QStandardItemH; cdecl; external QtIntf name 'QStandardItemModel_itemPrototype';
procedure QStandardItemModel_setItemPrototype(handle: QStandardItemModelH; item: QStandardItemH); cdecl; external QtIntf name 'QStandardItemModel_setItemPrototype';
procedure QStandardItemModel_findItems(handle: QStandardItemModelH; retval: PIntArray; text: PWideString; flags: QtMatchFlags = QtMatchExactly; column: Integer = 0); cdecl; external QtIntf name 'QStandardItemModel_findItems';
function QStandardItemModel_sortRole(handle: QStandardItemModelH): Integer; cdecl; external QtIntf name 'QStandardItemModel_sortRole';
procedure QStandardItemModel_setSortRole(handle: QStandardItemModelH; role: Integer); cdecl; external QtIntf name 'QStandardItemModel_setSortRole';


type
  QStandardItemModel_itemChanged_Event = procedure (item: QStandardItemH) of object cdecl;



type
  QAbstractItemDelegateEndEditHint = ( // QAbstractItemDelegate::EndEditHint (1)
    QAbstractItemDelegateNoHint, QAbstractItemDelegateEditNextItem, QAbstractItemDelegateEditPreviousItem, QAbstractItemDelegateSubmitModelCache, QAbstractItemDelegateRevertModelCache );

procedure QAbstractItemDelegate_paint(handle: QAbstractItemDelegateH; painter: QPainterH; option: QStyleOptionViewItemH; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemDelegate_paint';
procedure QAbstractItemDelegate_sizeHint(handle: QAbstractItemDelegateH; retval: PSize; option: QStyleOptionViewItemH; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemDelegate_sizeHint';
function QAbstractItemDelegate_createEditor(handle: QAbstractItemDelegateH; parent: QWidgetH; option: QStyleOptionViewItemH; index: QModelIndexH): QWidgetH; cdecl; external QtIntf name 'QAbstractItemDelegate_createEditor';
procedure QAbstractItemDelegate_setEditorData(handle: QAbstractItemDelegateH; editor: QWidgetH; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemDelegate_setEditorData';
procedure QAbstractItemDelegate_setModelData(handle: QAbstractItemDelegateH; editor: QWidgetH; model: QAbstractItemModelH; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemDelegate_setModelData';
procedure QAbstractItemDelegate_updateEditorGeometry(handle: QAbstractItemDelegateH; editor: QWidgetH; option: QStyleOptionViewItemH; index: QModelIndexH); cdecl; external QtIntf name 'QAbstractItemDelegate_updateEditorGeometry';
function QAbstractItemDelegate_editorEvent(handle: QAbstractItemDelegateH; event: QEventH; model: QAbstractItemModelH; option: QStyleOptionViewItemH; index: QModelIndexH): Boolean; cdecl; external QtIntf name 'QAbstractItemDelegate_editorEvent';
procedure QAbstractItemDelegate_elidedText(retval: PWideString; fontMetrics: QFontMetricsH; width: Integer; mode: QtTextElideMode; text: PWideString); cdecl; external QtIntf name 'QAbstractItemDelegate_elidedText';


type
  QAbstractItemDelegate_commitData_Event = procedure (editor: QWidgetH) of object cdecl;
  QAbstractItemDelegate_closeEditor_Event = procedure (editor: QWidgetH; hint: QAbstractItemDelegateEndEditHint = QAbstractItemDelegateNoHint) of object cdecl;
  QAbstractItemDelegate_closeEditor2_Event = procedure (editor: QWidgetH) of object cdecl;


function QItemDelegate_create(parent: QObjectH = nil): QItemDelegateH; cdecl; external QtIntf name 'QItemDelegate_create';
procedure QItemDelegate_destroy(handle: QItemDelegateH); cdecl; external QtIntf name 'QItemDelegate_destroy'; 
function QItemDelegate_hasClipping(handle: QItemDelegateH): Boolean; cdecl; external QtIntf name 'QItemDelegate_hasClipping';
procedure QItemDelegate_setClipping(handle: QItemDelegateH; clip: Boolean); cdecl; external QtIntf name 'QItemDelegate_setClipping';
procedure QItemDelegate_paint(handle: QItemDelegateH; painter: QPainterH; option: QStyleOptionViewItemH; index: QModelIndexH); cdecl; external QtIntf name 'QItemDelegate_paint';
procedure QItemDelegate_sizeHint(handle: QItemDelegateH; retval: PSize; option: QStyleOptionViewItemH; index: QModelIndexH); cdecl; external QtIntf name 'QItemDelegate_sizeHint';
function QItemDelegate_createEditor(handle: QItemDelegateH; parent: QWidgetH; option: QStyleOptionViewItemH; index: QModelIndexH): QWidgetH; cdecl; external QtIntf name 'QItemDelegate_createEditor';
procedure QItemDelegate_setEditorData(handle: QItemDelegateH; editor: QWidgetH; index: QModelIndexH); cdecl; external QtIntf name 'QItemDelegate_setEditorData';
procedure QItemDelegate_setModelData(handle: QItemDelegateH; editor: QWidgetH; model: QAbstractItemModelH; index: QModelIndexH); cdecl; external QtIntf name 'QItemDelegate_setModelData';
procedure QItemDelegate_updateEditorGeometry(handle: QItemDelegateH; editor: QWidgetH; option: QStyleOptionViewItemH; index: QModelIndexH); cdecl; external QtIntf name 'QItemDelegate_updateEditorGeometry';
function QItemDelegate_itemEditorFactory(handle: QItemDelegateH): QItemEditorFactoryH; cdecl; external QtIntf name 'QItemDelegate_itemEditorFactory';
procedure QItemDelegate_setItemEditorFactory(handle: QItemDelegateH; factory: QItemEditorFactoryH); cdecl; external QtIntf name 'QItemDelegate_setItemEditorFactory';

function QTableView_create(parent: QWidgetH = nil): QTableViewH; cdecl; external QtIntf name 'QTableView_create';
procedure QTableView_destroy(handle: QTableViewH); cdecl; external QtIntf name 'QTableView_destroy'; 
procedure QTableView_setModel(handle: QTableViewH; model: QAbstractItemModelH); cdecl; external QtIntf name 'QTableView_setModel';
procedure QTableView_setRootIndex(handle: QTableViewH; index: QModelIndexH); cdecl; external QtIntf name 'QTableView_setRootIndex';
procedure QTableView_setSelectionModel(handle: QTableViewH; selectionModel: QItemSelectionModelH); cdecl; external QtIntf name 'QTableView_setSelectionModel';
function QTableView_horizontalHeader(handle: QTableViewH): QHeaderViewH; cdecl; external QtIntf name 'QTableView_horizontalHeader';
function QTableView_verticalHeader(handle: QTableViewH): QHeaderViewH; cdecl; external QtIntf name 'QTableView_verticalHeader';
procedure QTableView_setHorizontalHeader(handle: QTableViewH; header: QHeaderViewH); cdecl; external QtIntf name 'QTableView_setHorizontalHeader';
procedure QTableView_setVerticalHeader(handle: QTableViewH; header: QHeaderViewH); cdecl; external QtIntf name 'QTableView_setVerticalHeader';
function QTableView_rowViewportPosition(handle: QTableViewH; row: Integer): Integer; cdecl; external QtIntf name 'QTableView_rowViewportPosition';
function QTableView_rowAt(handle: QTableViewH; y: Integer): Integer; cdecl; external QtIntf name 'QTableView_rowAt';
procedure QTableView_setRowHeight(handle: QTableViewH; row: Integer; height: Integer); cdecl; external QtIntf name 'QTableView_setRowHeight';
function QTableView_rowHeight(handle: QTableViewH; row: Integer): Integer; cdecl; external QtIntf name 'QTableView_rowHeight';
function QTableView_columnViewportPosition(handle: QTableViewH; column: Integer): Integer; cdecl; external QtIntf name 'QTableView_columnViewportPosition';
function QTableView_columnAt(handle: QTableViewH; x: Integer): Integer; cdecl; external QtIntf name 'QTableView_columnAt';
procedure QTableView_setColumnWidth(handle: QTableViewH; column: Integer; width: Integer); cdecl; external QtIntf name 'QTableView_setColumnWidth';
function QTableView_columnWidth(handle: QTableViewH; column: Integer): Integer; cdecl; external QtIntf name 'QTableView_columnWidth';
function QTableView_isRowHidden(handle: QTableViewH; row: Integer): Boolean; cdecl; external QtIntf name 'QTableView_isRowHidden';
procedure QTableView_setRowHidden(handle: QTableViewH; row: Integer; hide: Boolean); cdecl; external QtIntf name 'QTableView_setRowHidden';
function QTableView_isColumnHidden(handle: QTableViewH; column: Integer): Boolean; cdecl; external QtIntf name 'QTableView_isColumnHidden';
procedure QTableView_setColumnHidden(handle: QTableViewH; column: Integer; hide: Boolean); cdecl; external QtIntf name 'QTableView_setColumnHidden';
procedure QTableView_setSortingEnabled(handle: QTableViewH; enable: Boolean); cdecl; external QtIntf name 'QTableView_setSortingEnabled';
function QTableView_isSortingEnabled(handle: QTableViewH): Boolean; cdecl; external QtIntf name 'QTableView_isSortingEnabled';
function QTableView_showGrid(handle: QTableViewH): Boolean; cdecl; external QtIntf name 'QTableView_showGrid';
function QTableView_gridStyle(handle: QTableViewH): QtPenStyle; cdecl; external QtIntf name 'QTableView_gridStyle';
procedure QTableView_setGridStyle(handle: QTableViewH; style: QtPenStyle); cdecl; external QtIntf name 'QTableView_setGridStyle';
procedure QTableView_visualRect(handle: QTableViewH; retval: PRect; index: QModelIndexH); cdecl; external QtIntf name 'QTableView_visualRect';
procedure QTableView_scrollTo(handle: QTableViewH; index: QModelIndexH; hint: QAbstractItemViewScrollHint); cdecl; external QtIntf name 'QTableView_scrollTo';
procedure QTableView_indexAt(handle: QTableViewH; retval: QModelIndexH; p: PQtPoint); cdecl; external QtIntf name 'QTableView_indexAt';
procedure QTableView_setSpan(handle: QTableViewH; row: Integer; column: Integer; rowSpan: Integer; columnSpan: Integer); cdecl; external QtIntf name 'QTableView_setSpan';
function QTableView_rowSpan(handle: QTableViewH; row: Integer; column: Integer): Integer; cdecl; external QtIntf name 'QTableView_rowSpan';
function QTableView_columnSpan(handle: QTableViewH; row: Integer; column: Integer): Integer; cdecl; external QtIntf name 'QTableView_columnSpan';
procedure QTableView_sortByColumn(handle: QTableViewH; column: Integer; order: QtSortOrder); overload; cdecl; external QtIntf name 'QTableView_sortByColumn';
procedure QTableView_selectRow(handle: QTableViewH; row: Integer); cdecl; external QtIntf name 'QTableView_selectRow';
procedure QTableView_selectColumn(handle: QTableViewH; column: Integer); cdecl; external QtIntf name 'QTableView_selectColumn';
procedure QTableView_hideRow(handle: QTableViewH; row: Integer); cdecl; external QtIntf name 'QTableView_hideRow';
procedure QTableView_hideColumn(handle: QTableViewH; column: Integer); cdecl; external QtIntf name 'QTableView_hideColumn';
procedure QTableView_showRow(handle: QTableViewH; row: Integer); cdecl; external QtIntf name 'QTableView_showRow';
procedure QTableView_showColumn(handle: QTableViewH; column: Integer); cdecl; external QtIntf name 'QTableView_showColumn';
procedure QTableView_resizeRowToContents(handle: QTableViewH; row: Integer); cdecl; external QtIntf name 'QTableView_resizeRowToContents';
procedure QTableView_resizeRowsToContents(handle: QTableViewH); cdecl; external QtIntf name 'QTableView_resizeRowsToContents';
procedure QTableView_resizeColumnToContents(handle: QTableViewH; column: Integer); cdecl; external QtIntf name 'QTableView_resizeColumnToContents';
procedure QTableView_resizeColumnsToContents(handle: QTableViewH); cdecl; external QtIntf name 'QTableView_resizeColumnsToContents';
procedure QTableView_sortByColumn(handle: QTableViewH; column: Integer); overload; cdecl; external QtIntf name 'QTableView_sortByColumn2';
procedure QTableView_setShowGrid(handle: QTableViewH; show: Boolean); cdecl; external QtIntf name 'QTableView_setShowGrid';


type
  QTableWidgetItemItemType = (  //QTableWidgetItem::ItemType (2s)
    QTableWidgetItemType = 0,
    QTableWidgetItemUserType = 1000 );

function QTableWidgetSelectionRange_create(): QTableWidgetSelectionRangeH; overload; cdecl; external QtIntf name 'QTableWidgetSelectionRange_create';
procedure QTableWidgetSelectionRange_destroy(handle: QTableWidgetSelectionRangeH); cdecl; external QtIntf name 'QTableWidgetSelectionRange_destroy'; 
function QTableWidgetSelectionRange_create(top: Integer; left: Integer; bottom: Integer; right: Integer): QTableWidgetSelectionRangeH; overload; cdecl; external QtIntf name 'QTableWidgetSelectionRange_create2';
function QTableWidgetSelectionRange_create(other: QTableWidgetSelectionRangeH): QTableWidgetSelectionRangeH; overload; cdecl; external QtIntf name 'QTableWidgetSelectionRange_create3';
function QTableWidgetSelectionRange_topRow(handle: QTableWidgetSelectionRangeH): Integer; cdecl; external QtIntf name 'QTableWidgetSelectionRange_topRow';
function QTableWidgetSelectionRange_bottomRow(handle: QTableWidgetSelectionRangeH): Integer; cdecl; external QtIntf name 'QTableWidgetSelectionRange_bottomRow';
function QTableWidgetSelectionRange_leftColumn(handle: QTableWidgetSelectionRangeH): Integer; cdecl; external QtIntf name 'QTableWidgetSelectionRange_leftColumn';
function QTableWidgetSelectionRange_rightColumn(handle: QTableWidgetSelectionRangeH): Integer; cdecl; external QtIntf name 'QTableWidgetSelectionRange_rightColumn';
function QTableWidgetSelectionRange_rowCount(handle: QTableWidgetSelectionRangeH): Integer; cdecl; external QtIntf name 'QTableWidgetSelectionRange_rowCount';
function QTableWidgetSelectionRange_columnCount(handle: QTableWidgetSelectionRangeH): Integer; cdecl; external QtIntf name 'QTableWidgetSelectionRange_columnCount';

function QTableWidgetItem_create(_type: Integer = QTableWidgetItemType): QTableWidgetItemH; overload; cdecl; external QtIntf name 'QTableWidgetItem_create';
procedure QTableWidgetItem_destroy(handle: QTableWidgetItemH); cdecl; external QtIntf name 'QTableWidgetItem_destroy'; 
function QTableWidgetItem_create(text: PWideString; _type: Integer = QTableWidgetItemType): QTableWidgetItemH; overload; cdecl; external QtIntf name 'QTableWidgetItem_create2';
function QTableWidgetItem_create(icon: QIconH; text: PWideString; _type: Integer = QTableWidgetItemType): QTableWidgetItemH; overload; cdecl; external QtIntf name 'QTableWidgetItem_create3';
function QTableWidgetItem_create(other: QTableWidgetItemH): QTableWidgetItemH; overload; cdecl; external QtIntf name 'QTableWidgetItem_create4';
function QTableWidgetItem_clone(handle: QTableWidgetItemH): QTableWidgetItemH; cdecl; external QtIntf name 'QTableWidgetItem_clone';
function QTableWidgetItem_tableWidget(handle: QTableWidgetItemH): QTableWidgetH; cdecl; external QtIntf name 'QTableWidgetItem_tableWidget';
function QTableWidgetItem_row(handle: QTableWidgetItemH): Integer; cdecl; external QtIntf name 'QTableWidgetItem_row';
function QTableWidgetItem_column(handle: QTableWidgetItemH): Integer; cdecl; external QtIntf name 'QTableWidgetItem_column';
procedure QTableWidgetItem_setSelected(handle: QTableWidgetItemH; select: Boolean); cdecl; external QtIntf name 'QTableWidgetItem_setSelected';
function QTableWidgetItem_isSelected(handle: QTableWidgetItemH): Boolean; cdecl; external QtIntf name 'QTableWidgetItem_isSelected';
function QTableWidgetItem_flags(handle: QTableWidgetItemH): QtItemFlags; cdecl; external QtIntf name 'QTableWidgetItem_flags';
procedure QTableWidgetItem_setFlags(handle: QTableWidgetItemH; flags: QtItemFlags); cdecl; external QtIntf name 'QTableWidgetItem_setFlags';
procedure QTableWidgetItem_text(handle: QTableWidgetItemH; retval: PWideString); cdecl; external QtIntf name 'QTableWidgetItem_text';
procedure QTableWidgetItem_setText(handle: QTableWidgetItemH; text: PWideString); cdecl; external QtIntf name 'QTableWidgetItem_setText';
procedure QTableWidgetItem_icon(handle: QTableWidgetItemH; retval: QIconH); cdecl; external QtIntf name 'QTableWidgetItem_icon';
procedure QTableWidgetItem_setIcon(handle: QTableWidgetItemH; icon: QIconH); cdecl; external QtIntf name 'QTableWidgetItem_setIcon';
procedure QTableWidgetItem_statusTip(handle: QTableWidgetItemH; retval: PWideString); cdecl; external QtIntf name 'QTableWidgetItem_statusTip';
procedure QTableWidgetItem_setStatusTip(handle: QTableWidgetItemH; statusTip: PWideString); cdecl; external QtIntf name 'QTableWidgetItem_setStatusTip';
procedure QTableWidgetItem_toolTip(handle: QTableWidgetItemH; retval: PWideString); cdecl; external QtIntf name 'QTableWidgetItem_toolTip';
procedure QTableWidgetItem_setToolTip(handle: QTableWidgetItemH; toolTip: PWideString); cdecl; external QtIntf name 'QTableWidgetItem_setToolTip';
procedure QTableWidgetItem_whatsThis(handle: QTableWidgetItemH; retval: PWideString); cdecl; external QtIntf name 'QTableWidgetItem_whatsThis';
procedure QTableWidgetItem_setWhatsThis(handle: QTableWidgetItemH; whatsThis: PWideString); cdecl; external QtIntf name 'QTableWidgetItem_setWhatsThis';
procedure QTableWidgetItem_font(handle: QTableWidgetItemH; retval: QFontH); cdecl; external QtIntf name 'QTableWidgetItem_font';
procedure QTableWidgetItem_setFont(handle: QTableWidgetItemH; font: QFontH); cdecl; external QtIntf name 'QTableWidgetItem_setFont';
function QTableWidgetItem_textAlignment(handle: QTableWidgetItemH): Integer; cdecl; external QtIntf name 'QTableWidgetItem_textAlignment';
procedure QTableWidgetItem_setTextAlignment(handle: QTableWidgetItemH; alignment: Integer); cdecl; external QtIntf name 'QTableWidgetItem_setTextAlignment';
procedure QTableWidgetItem_backgroundColor(handle: QTableWidgetItemH; retval: PQColor); cdecl; external QtIntf name 'QTableWidgetItem_backgroundColor';
procedure QTableWidgetItem_setBackgroundColor(handle: QTableWidgetItemH; color: PQColor); cdecl; external QtIntf name 'QTableWidgetItem_setBackgroundColor';
procedure QTableWidgetItem_background(handle: QTableWidgetItemH; retval: QBrushH); cdecl; external QtIntf name 'QTableWidgetItem_background';
procedure QTableWidgetItem_setBackground(handle: QTableWidgetItemH; brush: QBrushH); cdecl; external QtIntf name 'QTableWidgetItem_setBackground';
procedure QTableWidgetItem_textColor(handle: QTableWidgetItemH; retval: PQColor); cdecl; external QtIntf name 'QTableWidgetItem_textColor';
procedure QTableWidgetItem_setTextColor(handle: QTableWidgetItemH; color: PQColor); cdecl; external QtIntf name 'QTableWidgetItem_setTextColor';
procedure QTableWidgetItem_foreground(handle: QTableWidgetItemH; retval: QBrushH); cdecl; external QtIntf name 'QTableWidgetItem_foreground';
procedure QTableWidgetItem_setForeground(handle: QTableWidgetItemH; brush: QBrushH); cdecl; external QtIntf name 'QTableWidgetItem_setForeground';
function QTableWidgetItem_checkState(handle: QTableWidgetItemH): QtCheckState; cdecl; external QtIntf name 'QTableWidgetItem_checkState';
procedure QTableWidgetItem_setCheckState(handle: QTableWidgetItemH; state: QtCheckState); cdecl; external QtIntf name 'QTableWidgetItem_setCheckState';
procedure QTableWidgetItem_sizeHint(handle: QTableWidgetItemH; retval: PSize); cdecl; external QtIntf name 'QTableWidgetItem_sizeHint';
procedure QTableWidgetItem_setSizeHint(handle: QTableWidgetItemH; size: PSize); cdecl; external QtIntf name 'QTableWidgetItem_setSizeHint';
procedure QTableWidgetItem_data(handle: QTableWidgetItemH; retval: QVariantH; role: Integer); cdecl; external QtIntf name 'QTableWidgetItem_data';
procedure QTableWidgetItem_setData(handle: QTableWidgetItemH; role: Integer; value: QVariantH); cdecl; external QtIntf name 'QTableWidgetItem_setData';
procedure QTableWidgetItem_read(handle: QTableWidgetItemH; _in: QDataStreamH); cdecl; external QtIntf name 'QTableWidgetItem_read';
procedure QTableWidgetItem_write(handle: QTableWidgetItemH; _out: QDataStreamH); cdecl; external QtIntf name 'QTableWidgetItem_write';
function QTableWidgetItem_type(handle: QTableWidgetItemH): Integer; cdecl; external QtIntf name 'QTableWidgetItem_type';

function QTableWidget_create(parent: QWidgetH = nil): QTableWidgetH; overload; cdecl; external QtIntf name 'QTableWidget_create';
procedure QTableWidget_destroy(handle: QTableWidgetH); cdecl; external QtIntf name 'QTableWidget_destroy'; 
function QTableWidget_create(rows: Integer; columns: Integer; parent: QWidgetH = nil): QTableWidgetH; overload; cdecl; external QtIntf name 'QTableWidget_create2';
procedure QTableWidget_setRowCount(handle: QTableWidgetH; rows: Integer); cdecl; external QtIntf name 'QTableWidget_setRowCount';
function QTableWidget_rowCount(handle: QTableWidgetH): Integer; cdecl; external QtIntf name 'QTableWidget_rowCount';
procedure QTableWidget_setColumnCount(handle: QTableWidgetH; columns: Integer); cdecl; external QtIntf name 'QTableWidget_setColumnCount';
function QTableWidget_columnCount(handle: QTableWidgetH): Integer; cdecl; external QtIntf name 'QTableWidget_columnCount';
function QTableWidget_row(handle: QTableWidgetH; item: QTableWidgetItemH): Integer; cdecl; external QtIntf name 'QTableWidget_row';
function QTableWidget_column(handle: QTableWidgetH; item: QTableWidgetItemH): Integer; cdecl; external QtIntf name 'QTableWidget_column';
function QTableWidget_item(handle: QTableWidgetH; row: Integer; column: Integer): QTableWidgetItemH; cdecl; external QtIntf name 'QTableWidget_item';
procedure QTableWidget_setItem(handle: QTableWidgetH; row: Integer; column: Integer; item: QTableWidgetItemH); cdecl; external QtIntf name 'QTableWidget_setItem';
function QTableWidget_takeItem(handle: QTableWidgetH; row: Integer; column: Integer): QTableWidgetItemH; cdecl; external QtIntf name 'QTableWidget_takeItem';
function QTableWidget_verticalHeaderItem(handle: QTableWidgetH; row: Integer): QTableWidgetItemH; cdecl; external QtIntf name 'QTableWidget_verticalHeaderItem';
procedure QTableWidget_setVerticalHeaderItem(handle: QTableWidgetH; row: Integer; item: QTableWidgetItemH); cdecl; external QtIntf name 'QTableWidget_setVerticalHeaderItem';
function QTableWidget_takeVerticalHeaderItem(handle: QTableWidgetH; row: Integer): QTableWidgetItemH; cdecl; external QtIntf name 'QTableWidget_takeVerticalHeaderItem';
function QTableWidget_horizontalHeaderItem(handle: QTableWidgetH; column: Integer): QTableWidgetItemH; cdecl; external QtIntf name 'QTableWidget_horizontalHeaderItem';
procedure QTableWidget_setHorizontalHeaderItem(handle: QTableWidgetH; column: Integer; item: QTableWidgetItemH); cdecl; external QtIntf name 'QTableWidget_setHorizontalHeaderItem';
function QTableWidget_takeHorizontalHeaderItem(handle: QTableWidgetH; column: Integer): QTableWidgetItemH; cdecl; external QtIntf name 'QTableWidget_takeHorizontalHeaderItem';
procedure QTableWidget_setVerticalHeaderLabels(handle: QTableWidgetH; labels: QStringListH); cdecl; external QtIntf name 'QTableWidget_setVerticalHeaderLabels';
procedure QTableWidget_setHorizontalHeaderLabels(handle: QTableWidgetH; labels: QStringListH); cdecl; external QtIntf name 'QTableWidget_setHorizontalHeaderLabels';
function QTableWidget_currentRow(handle: QTableWidgetH): Integer; cdecl; external QtIntf name 'QTableWidget_currentRow';
function QTableWidget_currentColumn(handle: QTableWidgetH): Integer; cdecl; external QtIntf name 'QTableWidget_currentColumn';
function QTableWidget_currentItem(handle: QTableWidgetH): QTableWidgetItemH; cdecl; external QtIntf name 'QTableWidget_currentItem';
procedure QTableWidget_setCurrentItem(handle: QTableWidgetH; item: QTableWidgetItemH); cdecl; external QtIntf name 'QTableWidget_setCurrentItem';
procedure QTableWidget_setCurrentCell(handle: QTableWidgetH; row: Integer; column: Integer); cdecl; external QtIntf name 'QTableWidget_setCurrentCell';
procedure QTableWidget_sortItems(handle: QTableWidgetH; column: Integer; order: QtSortOrder = QtAscendingOrder); cdecl; external QtIntf name 'QTableWidget_sortItems';
procedure QTableWidget_setSortingEnabled(handle: QTableWidgetH; enable: Boolean); cdecl; external QtIntf name 'QTableWidget_setSortingEnabled';
function QTableWidget_isSortingEnabled(handle: QTableWidgetH): Boolean; cdecl; external QtIntf name 'QTableWidget_isSortingEnabled';
procedure QTableWidget_editItem(handle: QTableWidgetH; item: QTableWidgetItemH); cdecl; external QtIntf name 'QTableWidget_editItem';
procedure QTableWidget_openPersistentEditor(handle: QTableWidgetH; item: QTableWidgetItemH); cdecl; external QtIntf name 'QTableWidget_openPersistentEditor';
procedure QTableWidget_closePersistentEditor(handle: QTableWidgetH; item: QTableWidgetItemH); cdecl; external QtIntf name 'QTableWidget_closePersistentEditor';
function QTableWidget_cellWidget(handle: QTableWidgetH; row: Integer; column: Integer): QWidgetH; cdecl; external QtIntf name 'QTableWidget_cellWidget';
procedure QTableWidget_setCellWidget(handle: QTableWidgetH; row: Integer; column: Integer; widget: QWidgetH); cdecl; external QtIntf name 'QTableWidget_setCellWidget';
function QTableWidget_isItemSelected(handle: QTableWidgetH; item: QTableWidgetItemH): Boolean; cdecl; external QtIntf name 'QTableWidget_isItemSelected';
procedure QTableWidget_setItemSelected(handle: QTableWidgetH; item: QTableWidgetItemH; select: Boolean); cdecl; external QtIntf name 'QTableWidget_setItemSelected';
procedure QTableWidget_setRangeSelected(handle: QTableWidgetH; range: QTableWidgetSelectionRangeH; select: Boolean); cdecl; external QtIntf name 'QTableWidget_setRangeSelected';
function QTableWidget_visualRow(handle: QTableWidgetH; logicalRow: Integer): Integer; cdecl; external QtIntf name 'QTableWidget_visualRow';
function QTableWidget_visualColumn(handle: QTableWidgetH; logicalColumn: Integer): Integer; cdecl; external QtIntf name 'QTableWidget_visualColumn';
function QTableWidget_itemAt(handle: QTableWidgetH; p: PQtPoint): QTableWidgetItemH; overload; cdecl; external QtIntf name 'QTableWidget_itemAt';
function QTableWidget_itemAt(handle: QTableWidgetH; x: Integer; y: Integer): QTableWidgetItemH; overload; cdecl; external QtIntf name 'QTableWidget_itemAt2';
procedure QTableWidget_visualItemRect(handle: QTableWidgetH; retval: PRect; item: QTableWidgetItemH); cdecl; external QtIntf name 'QTableWidget_visualItemRect';
function QTableWidget_itemPrototype(handle: QTableWidgetH): QTableWidgetItemH; cdecl; external QtIntf name 'QTableWidget_itemPrototype';
procedure QTableWidget_setItemPrototype(handle: QTableWidgetH; item: QTableWidgetItemH); cdecl; external QtIntf name 'QTableWidget_setItemPrototype';
procedure QTableWidget_scrollToItem(handle: QTableWidgetH; item: QTableWidgetItemH; hint: QAbstractItemViewScrollHint); cdecl; external QtIntf name 'QTableWidget_scrollToItem';
procedure QTableWidget_insertRow(handle: QTableWidgetH; row: Integer); cdecl; external QtIntf name 'QTableWidget_insertRow';
procedure QTableWidget_insertColumn(handle: QTableWidgetH; column: Integer); cdecl; external QtIntf name 'QTableWidget_insertColumn';
procedure QTableWidget_removeRow(handle: QTableWidgetH; row: Integer); cdecl; external QtIntf name 'QTableWidget_removeRow';
procedure QTableWidget_removeColumn(handle: QTableWidgetH; column: Integer); cdecl; external QtIntf name 'QTableWidget_removeColumn';
procedure QTableWidget_clear(handle: QTableWidgetH); cdecl; external QtIntf name 'QTableWidget_clear';
procedure QTableWidget_clearContents(handle: QTableWidgetH); cdecl; external QtIntf name 'QTableWidget_clearContents';


type
  QTableWidget_itemPressed_Event = procedure (item: QTableWidgetItemH) of object cdecl;
  QTableWidget_itemClicked_Event = procedure (item: QTableWidgetItemH) of object cdecl;
  QTableWidget_itemDoubleClicked_Event = procedure (item: QTableWidgetItemH) of object cdecl;
  QTableWidget_itemActivated_Event = procedure (item: QTableWidgetItemH) of object cdecl;
  QTableWidget_itemEntered_Event = procedure (item: QTableWidgetItemH) of object cdecl;
  QTableWidget_itemChanged_Event = procedure (item: QTableWidgetItemH) of object cdecl;
  QTableWidget_currentItemChanged_Event = procedure (current: QTableWidgetItemH; previous: QTableWidgetItemH) of object cdecl;
  QTableWidget_itemSelectionChanged_Event = procedure () of object cdecl;
  QTableWidget_cellPressed_Event = procedure (row: Integer; column: Integer) of object cdecl;
  QTableWidget_cellClicked_Event = procedure (row: Integer; column: Integer) of object cdecl;
  QTableWidget_cellDoubleClicked_Event = procedure (row: Integer; column: Integer) of object cdecl;
  QTableWidget_cellActivated_Event = procedure (row: Integer; column: Integer) of object cdecl;
  QTableWidget_cellEntered_Event = procedure (row: Integer; column: Integer) of object cdecl;
  QTableWidget_cellChanged_Event = procedure (row: Integer; column: Integer) of object cdecl;
  QTableWidget_currentCellChanged_Event = procedure (currentRow: Integer; currentColumn: Integer; previousRow: Integer; previousColumn: Integer) of object cdecl;


function QItemEditorCreatorBase_createWidget(handle: QItemEditorCreatorBaseH; parent: QWidgetH): QWidgetH; cdecl; external QtIntf name 'QItemEditorCreatorBase_createWidget';
procedure QItemEditorCreatorBase_valuePropertyName(handle: QItemEditorCreatorBaseH; retval: QByteArrayH); cdecl; external QtIntf name 'QItemEditorCreatorBase_valuePropertyName';

function QItemEditorFactory_create(): QItemEditorFactoryH; cdecl; external QtIntf name 'QItemEditorFactory_create';
procedure QItemEditorFactory_destroy(handle: QItemEditorFactoryH); cdecl; external QtIntf name 'QItemEditorFactory_destroy'; 
function QItemEditorFactory_createEditor(handle: QItemEditorFactoryH; _type: QVariantType; parent: QWidgetH): QWidgetH; cdecl; external QtIntf name 'QItemEditorFactory_createEditor';
procedure QItemEditorFactory_valuePropertyName(handle: QItemEditorFactoryH; retval: QByteArrayH; _type: QVariantType); cdecl; external QtIntf name 'QItemEditorFactory_valuePropertyName';
procedure QItemEditorFactory_registerEditor(handle: QItemEditorFactoryH; _type: QVariantType; creator: QItemEditorCreatorBaseH); cdecl; external QtIntf name 'QItemEditorFactory_registerEditor';
function QItemEditorFactory_defaultFactory(): QItemEditorFactoryH; cdecl; external QtIntf name 'QItemEditorFactory_defaultFactory';
procedure QItemEditorFactory_setDefaultFactory(factory: QItemEditorFactoryH); cdecl; external QtIntf name 'QItemEditorFactory_setDefaultFactory';

type
  QItemSelectionModelSelectionFlag = cardinal; //  QItemSelectionModel::SelectionFlag (4)
  QItemSelectionModelSelectionFlags = QItemSelectionModelSelectionFlag; // QFlags<>

const
    QItemSelectionModelNoUpdate = 0 { $0 };
    QItemSelectionModelClear = 1 { $1 };
    QItemSelectionModelSelect = 2 { $2 };
    QItemSelectionModelDeselect = 4 { $4 };
    QItemSelectionModelToggle = 8 { $8 };
    QItemSelectionModelCurrent = 16 { $10 };
    QItemSelectionModelRows = 32 { $20 };
    QItemSelectionModelColumns = 64 { $40 };
    QItemSelectionModelSelectCurrent = 18 { $12 };
    QItemSelectionModelToggleCurrent = 24 { $18 };
    QItemSelectionModelClearAndSelect = 3 { $3 };


function QItemSelectionRange_create(): QItemSelectionRangeH; overload; cdecl; external QtIntf name 'QItemSelectionRange_create';
procedure QItemSelectionRange_destroy(handle: QItemSelectionRangeH); cdecl; external QtIntf name 'QItemSelectionRange_destroy'; 
function QItemSelectionRange_create(other: QItemSelectionRangeH): QItemSelectionRangeH; overload; cdecl; external QtIntf name 'QItemSelectionRange_create2';
function QItemSelectionRange_create(topLeft: QModelIndexH; bottomRight: QModelIndexH): QItemSelectionRangeH; overload; cdecl; external QtIntf name 'QItemSelectionRange_create3';
function QItemSelectionRange_create(index: QModelIndexH): QItemSelectionRangeH; overload; cdecl; external QtIntf name 'QItemSelectionRange_create4';
function QItemSelectionRange_top(handle: QItemSelectionRangeH): Integer; cdecl; external QtIntf name 'QItemSelectionRange_top';
function QItemSelectionRange_left(handle: QItemSelectionRangeH): Integer; cdecl; external QtIntf name 'QItemSelectionRange_left';
function QItemSelectionRange_bottom(handle: QItemSelectionRangeH): Integer; cdecl; external QtIntf name 'QItemSelectionRange_bottom';
function QItemSelectionRange_right(handle: QItemSelectionRangeH): Integer; cdecl; external QtIntf name 'QItemSelectionRange_right';
function QItemSelectionRange_width(handle: QItemSelectionRangeH): Integer; cdecl; external QtIntf name 'QItemSelectionRange_width';
function QItemSelectionRange_height(handle: QItemSelectionRangeH): Integer; cdecl; external QtIntf name 'QItemSelectionRange_height';
procedure QItemSelectionRange_topLeft(handle: QItemSelectionRangeH; retval: QModelIndexH); cdecl; external QtIntf name 'QItemSelectionRange_topLeft';
procedure QItemSelectionRange_bottomRight(handle: QItemSelectionRangeH; retval: QModelIndexH); cdecl; external QtIntf name 'QItemSelectionRange_bottomRight';
procedure QItemSelectionRange_parent(handle: QItemSelectionRangeH; retval: QModelIndexH); cdecl; external QtIntf name 'QItemSelectionRange_parent';
function QItemSelectionRange_model(handle: QItemSelectionRangeH): QAbstractItemModelH; cdecl; external QtIntf name 'QItemSelectionRange_model';
function QItemSelectionRange_contains(handle: QItemSelectionRangeH; index: QModelIndexH): Boolean; overload; cdecl; external QtIntf name 'QItemSelectionRange_contains';
function QItemSelectionRange_contains(handle: QItemSelectionRangeH; row: Integer; column: Integer; parentIndex: QModelIndexH): Boolean; overload; cdecl; external QtIntf name 'QItemSelectionRange_contains2';
function QItemSelectionRange_intersects(handle: QItemSelectionRangeH; other: QItemSelectionRangeH): Boolean; cdecl; external QtIntf name 'QItemSelectionRange_intersects';
procedure QItemSelectionRange_intersect(handle: QItemSelectionRangeH; retval: QItemSelectionRangeH; other: QItemSelectionRangeH); cdecl; external QtIntf name 'QItemSelectionRange_intersect';
procedure QItemSelectionRange_intersected(handle: QItemSelectionRangeH; retval: QItemSelectionRangeH; other: QItemSelectionRangeH); cdecl; external QtIntf name 'QItemSelectionRange_intersected';
function QItemSelectionRange_isValid(handle: QItemSelectionRangeH): Boolean; cdecl; external QtIntf name 'QItemSelectionRange_isValid';

function QItemSelectionModel_create(model: QAbstractItemModelH): QItemSelectionModelH; overload; cdecl; external QtIntf name 'QItemSelectionModel_create';
procedure QItemSelectionModel_destroy(handle: QItemSelectionModelH); cdecl; external QtIntf name 'QItemSelectionModel_destroy'; 
function QItemSelectionModel_create(model: QAbstractItemModelH; parent: QObjectH): QItemSelectionModelH; overload; cdecl; external QtIntf name 'QItemSelectionModel_create2';
procedure QItemSelectionModel_currentIndex(handle: QItemSelectionModelH; retval: QModelIndexH); cdecl; external QtIntf name 'QItemSelectionModel_currentIndex';
function QItemSelectionModel_isSelected(handle: QItemSelectionModelH; index: QModelIndexH): Boolean; cdecl; external QtIntf name 'QItemSelectionModel_isSelected';
function QItemSelectionModel_isRowSelected(handle: QItemSelectionModelH; row: Integer; parent: QModelIndexH): Boolean; cdecl; external QtIntf name 'QItemSelectionModel_isRowSelected';
function QItemSelectionModel_isColumnSelected(handle: QItemSelectionModelH; column: Integer; parent: QModelIndexH): Boolean; cdecl; external QtIntf name 'QItemSelectionModel_isColumnSelected';
function QItemSelectionModel_rowIntersectsSelection(handle: QItemSelectionModelH; row: Integer; parent: QModelIndexH): Boolean; cdecl; external QtIntf name 'QItemSelectionModel_rowIntersectsSelection';
function QItemSelectionModel_columnIntersectsSelection(handle: QItemSelectionModelH; column: Integer; parent: QModelIndexH): Boolean; cdecl; external QtIntf name 'QItemSelectionModel_columnIntersectsSelection';
function QItemSelectionModel_hasSelection(handle: QItemSelectionModelH): Boolean; cdecl; external QtIntf name 'QItemSelectionModel_hasSelection';
function QItemSelectionModel_model(handle: QItemSelectionModelH): QAbstractItemModelH; cdecl; external QtIntf name 'QItemSelectionModel_model';
procedure QItemSelectionModel_setCurrentIndex(handle: QItemSelectionModelH; index: QModelIndexH; command: QItemSelectionModelSelectionFlags); cdecl; external QtIntf name 'QItemSelectionModel_setCurrentIndex';
procedure QItemSelectionModel_select(handle: QItemSelectionModelH; index: QModelIndexH; command: QItemSelectionModelSelectionFlags); cdecl; external QtIntf name 'QItemSelectionModel_select';
procedure QItemSelectionModel_clear(handle: QItemSelectionModelH); cdecl; external QtIntf name 'QItemSelectionModel_clear';
procedure QItemSelectionModel_reset(handle: QItemSelectionModelH); cdecl; external QtIntf name 'QItemSelectionModel_reset';
procedure QItemSelectionModel_clearSelection(handle: QItemSelectionModelH); cdecl; external QtIntf name 'QItemSelectionModel_clearSelection';


type
  QItemSelectionModel_currentChanged_Event = procedure (current: QModelIndexH; previous: QModelIndexH) of object cdecl;
  QItemSelectionModel_currentRowChanged_Event = procedure (current: QModelIndexH; previous: QModelIndexH) of object cdecl;
  QItemSelectionModel_currentColumnChanged_Event = procedure (current: QModelIndexH; previous: QModelIndexH) of object cdecl;


function QLCLItemDelegate_create(parent: QObjectH = nil): QLCLItemDelegateH; cdecl; external QtIntf name 'QLCLItemDelegate_create';
procedure QLCLItemDelegate_destroy(handle: QLCLItemDelegateH); cdecl; external QtIntf name 'QLCLItemDelegate_destroy'; 
procedure QLCLItemDelegate_override_sizeHint(handle: QLCLItemDelegateH; hook: QHookH); cdecl; external QtIntf name 'QLCLItemDelegate_override_sizeHint';
procedure QLCLItemDelegate_override_paint(handle: QLCLItemDelegateH; hook: QHookH); cdecl; external QtIntf name 'QLCLItemDelegate_override_paint';
function QLCLItemDelegate_override_createEditor(handle: QLCLItemDelegateH; hook: QHookH): QWidgetH; cdecl; external QtIntf name 'QLCLItemDelegate_override_createEditor';
procedure QLCLItemDelegate_override_setEditorData(handle: QLCLItemDelegateH; hook: QHookH); cdecl; external QtIntf name 'QLCLItemDelegate_override_setEditorData';
procedure QLCLItemDelegate_override_setModelData(handle: QLCLItemDelegateH; hook: QHookH); cdecl; external QtIntf name 'QLCLItemDelegate_override_setModelData';
procedure QLCLItemDelegate_override_updateEditorGeometry(handle: QLCLItemDelegateH; hook: QHookH); cdecl; external QtIntf name 'QLCLItemDelegate_override_updateEditorGeometry';
procedure QLCLItemDelegate_override_editorEvent(handle: QLCLItemDelegateH; hook: QHookH); cdecl; external QtIntf name 'QLCLItemDelegate_override_editorEvent';
function QLCLItemDelegate_InheritedEditorEvent(handle: QLCLItemDelegateH; event: QEventH; model: QAbstractItemModelH; option: QStyleOptionViewItemH; index: QModelIndexH): Boolean; cdecl; external QtIntf name 'QLCLItemDelegate_InheritedEditorEvent';


type
  QDialogDialogCode = ( // QDialog::DialogCode (1)
    QDialogRejected, QDialogAccepted );

function QDialog_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QDialogH; cdecl; external QtIntf name 'QDialog_create';
procedure QDialog_destroy(handle: QDialogH); cdecl; external QtIntf name 'QDialog_destroy'; 
function QDialog_result(handle: QDialogH): Integer; cdecl; external QtIntf name 'QDialog_result';
procedure QDialog_setVisible(handle: QDialogH; visible: Boolean); cdecl; external QtIntf name 'QDialog_setVisible';
procedure QDialog_setOrientation(handle: QDialogH; orientation: QtOrientation); cdecl; external QtIntf name 'QDialog_setOrientation';
function QDialog_orientation(handle: QDialogH): QtOrientation; cdecl; external QtIntf name 'QDialog_orientation';
procedure QDialog_setExtension(handle: QDialogH; extension: QWidgetH); cdecl; external QtIntf name 'QDialog_setExtension';
function QDialog_extension(handle: QDialogH): QWidgetH; cdecl; external QtIntf name 'QDialog_extension';
procedure QDialog_sizeHint(handle: QDialogH; retval: PSize); cdecl; external QtIntf name 'QDialog_sizeHint';
procedure QDialog_minimumSizeHint(handle: QDialogH; retval: PSize); cdecl; external QtIntf name 'QDialog_minimumSizeHint';
procedure QDialog_setSizeGripEnabled(handle: QDialogH; p1: Boolean); cdecl; external QtIntf name 'QDialog_setSizeGripEnabled';
function QDialog_isSizeGripEnabled(handle: QDialogH): Boolean; cdecl; external QtIntf name 'QDialog_isSizeGripEnabled';
procedure QDialog_setModal(handle: QDialogH; modal: Boolean); cdecl; external QtIntf name 'QDialog_setModal';
procedure QDialog_setResult(handle: QDialogH; r: Integer); cdecl; external QtIntf name 'QDialog_setResult';
function QDialog_exec(handle: QDialogH): Integer; cdecl; external QtIntf name 'QDialog_exec';
procedure QDialog_done(handle: QDialogH; p1: Integer); cdecl; external QtIntf name 'QDialog_done';
procedure QDialog_accept(handle: QDialogH); cdecl; external QtIntf name 'QDialog_accept';
procedure QDialog_reject(handle: QDialogH); cdecl; external QtIntf name 'QDialog_reject';
procedure QDialog_showExtension(handle: QDialogH; p1: Boolean); cdecl; external QtIntf name 'QDialog_showExtension';


type
  QDialog_finished_Event = procedure (result: Integer) of object cdecl;
  QDialog_accepted_Event = procedure () of object cdecl;
  QDialog_rejected_Event = procedure () of object cdecl;


procedure QFontDialog_getFont(retval: QFontH; ok: PBoolean; def: QFontH; parent: QWidgetH; caption: PWideString); overload; cdecl; external QtIntf name 'QFontDialog_getFont';
procedure QFontDialog_getFont(retval: QFontH; ok: PBoolean; def: QFontH; parent: QWidgetH = nil); overload; cdecl; external QtIntf name 'QFontDialog_getFont2';
procedure QFontDialog_getFont(retval: QFontH; ok: PBoolean; parent: QWidgetH = nil); overload; cdecl; external QtIntf name 'QFontDialog_getFont3';


type
  QMessageBoxIcon = (  //QMessageBox::Icon (2s)
    QMessageBoxNoIcon = 0,
    QMessageBoxInformation = 1,
    QMessageBoxWarning = 2,
    QMessageBoxCritical = 3,
    QMessageBoxQuestion = 4 );

  QMessageBoxButtonRole = (  //QMessageBox::ButtonRole (2)
    QMessageBoxInvalidRole = -1,
    QMessageBoxAcceptRole,
    QMessageBoxRejectRole,
    QMessageBoxDestructiveRole,
    QMessageBoxActionRole,
    QMessageBoxHelpRole,
    QMessageBoxYesRole,
    QMessageBoxNoRole,
    QMessageBoxResetRole,
    QMessageBoxApplyRole,
    QMessageBoxNRoles );

type
  QMessageBoxStandardButton = cardinal; //  QMessageBox::StandardButton (4)
  QMessageBoxStandardButtons = QMessageBoxStandardButton; // QFlags<>

const
    QMessageBoxNoButton = 0 { $0 };
    QMessageBoxOk = 1024 { $400 };
    QMessageBoxSave = 2048 { $800 };
    QMessageBoxSaveAll = 4096 { $1000 };
    QMessageBoxOpen = 8192 { $2000 };
    QMessageBoxYes = 16384 { $4000 };
    QMessageBoxYesToAll = 32768 { $8000 };
    QMessageBoxNo = 65536 { $10000 };
    QMessageBoxNoToAll = 131072 { $20000 };
    QMessageBoxAbort = 262144 { $40000 };
    QMessageBoxRetry = 524288 { $80000 };
    QMessageBoxIgnore = 1048576 { $100000 };
    QMessageBoxClose = 2097152 { $200000 };
    QMessageBoxCancel = 4194304 { $400000 };
    QMessageBoxDiscard = 8388608 { $800000 };
    QMessageBoxHelp = 16777216 { $1000000 };
    QMessageBoxApply = 33554432 { $2000000 };
    QMessageBoxReset = 67108864 { $4000000 };
    QMessageBoxRestoreDefaults = 134217728 { $8000000 };
    QMessageBoxFirstButton = 1024 { $400 };
    QMessageBoxLastButton = 134217728 { $8000000 };
    QMessageBoxYesAll = 32768 { $8000 };
    QMessageBoxNoAll = 131072 { $20000 };
    QMessageBoxDefault = 256 { $100 };
    QMessageBoxEscape = 512 { $200 };
    QMessageBoxFlagMask = 768 { $300 };
    QMessageBoxButtonMask = 4294966527 { $fffffcff };


function QMessageBox_create(parent: QWidgetH = nil): QMessageBoxH; overload; cdecl; external QtIntf name 'QMessageBox_create';
procedure QMessageBox_destroy(handle: QMessageBoxH); cdecl; external QtIntf name 'QMessageBox_destroy'; 
function QMessageBox_create(icon: QMessageBoxIcon; title: PWideString; text: PWideString; buttons: QMessageBoxStandardButtons = QMessageBoxNoButton; parent: QWidgetH = nil; f: QtWindowFlags = QtDialog or QtMSWindowsFixedSizeDialogHint): QMessageBoxH; overload; cdecl; external QtIntf name 'QMessageBox_create2';
procedure QMessageBox_addButton(handle: QMessageBoxH; button: QAbstractButtonH; role: QMessageBoxButtonRole); overload; cdecl; external QtIntf name 'QMessageBox_addButton';
function QMessageBox_addButton(handle: QMessageBoxH; text: PWideString; role: QMessageBoxButtonRole): QPushButtonH; overload; cdecl; external QtIntf name 'QMessageBox_addButton2';
function QMessageBox_addButton(handle: QMessageBoxH; button: QMessageBoxStandardButton): QPushButtonH; overload; cdecl; external QtIntf name 'QMessageBox_addButton3';
procedure QMessageBox_removeButton(handle: QMessageBoxH; button: QAbstractButtonH); cdecl; external QtIntf name 'QMessageBox_removeButton';
procedure QMessageBox_setStandardButtons(handle: QMessageBoxH; buttons: QMessageBoxStandardButtons); cdecl; external QtIntf name 'QMessageBox_setStandardButtons';
function QMessageBox_standardButtons(handle: QMessageBoxH): QMessageBoxStandardButtons; cdecl; external QtIntf name 'QMessageBox_standardButtons';
function QMessageBox_standardButton(handle: QMessageBoxH; button: QAbstractButtonH): QMessageBoxStandardButton; cdecl; external QtIntf name 'QMessageBox_standardButton';
function QMessageBox_button(handle: QMessageBoxH; which: QMessageBoxStandardButton): QAbstractButtonH; cdecl; external QtIntf name 'QMessageBox_button';
function QMessageBox_defaultButton(handle: QMessageBoxH): QPushButtonH; cdecl; external QtIntf name 'QMessageBox_defaultButton';
procedure QMessageBox_setDefaultButton(handle: QMessageBoxH; button: QPushButtonH); cdecl; external QtIntf name 'QMessageBox_setDefaultButton';
function QMessageBox_escapeButton(handle: QMessageBoxH): QAbstractButtonH; cdecl; external QtIntf name 'QMessageBox_escapeButton';
procedure QMessageBox_setEscapeButton(handle: QMessageBoxH; button: QAbstractButtonH); cdecl; external QtIntf name 'QMessageBox_setEscapeButton';
function QMessageBox_clickedButton(handle: QMessageBoxH): QAbstractButtonH; cdecl; external QtIntf name 'QMessageBox_clickedButton';
procedure QMessageBox_text(handle: QMessageBoxH; retval: PWideString); cdecl; external QtIntf name 'QMessageBox_text';
procedure QMessageBox_setText(handle: QMessageBoxH; text: PWideString); cdecl; external QtIntf name 'QMessageBox_setText';
function QMessageBox_icon(handle: QMessageBoxH): QMessageBoxIcon; cdecl; external QtIntf name 'QMessageBox_icon';
procedure QMessageBox_setIcon(handle: QMessageBoxH; p1: QMessageBoxIcon); cdecl; external QtIntf name 'QMessageBox_setIcon';
procedure QMessageBox_iconPixmap(handle: QMessageBoxH; retval: QPixmapH); cdecl; external QtIntf name 'QMessageBox_iconPixmap';
procedure QMessageBox_setIconPixmap(handle: QMessageBoxH; pixmap: QPixmapH); cdecl; external QtIntf name 'QMessageBox_setIconPixmap';
function QMessageBox_textFormat(handle: QMessageBoxH): QtTextFormat; cdecl; external QtIntf name 'QMessageBox_textFormat';
procedure QMessageBox_setTextFormat(handle: QMessageBoxH; format: QtTextFormat); cdecl; external QtIntf name 'QMessageBox_setTextFormat';
function QMessageBox_information(parent: QWidgetH; title: PWideString; text: PWideString; buttons: QMessageBoxStandardButtons = QMessageBoxOk; defaultButton: QMessageBoxStandardButton = QMessageBoxNoButton): QMessageBoxStandardButton; overload; cdecl; external QtIntf name 'QMessageBox_information';
function QMessageBox_question(parent: QWidgetH; title: PWideString; text: PWideString; buttons: QMessageBoxStandardButtons = QMessageBoxOk; defaultButton: QMessageBoxStandardButton = QMessageBoxNoButton): QMessageBoxStandardButton; overload; cdecl; external QtIntf name 'QMessageBox_question';
function QMessageBox_warning(parent: QWidgetH; title: PWideString; text: PWideString; buttons: QMessageBoxStandardButtons = QMessageBoxOk; defaultButton: QMessageBoxStandardButton = QMessageBoxNoButton): QMessageBoxStandardButton; overload; cdecl; external QtIntf name 'QMessageBox_warning';
function QMessageBox_critical(parent: QWidgetH; title: PWideString; text: PWideString; buttons: QMessageBoxStandardButtons = QMessageBoxOk; defaultButton: QMessageBoxStandardButton = QMessageBoxNoButton): QMessageBoxStandardButton; overload; cdecl; external QtIntf name 'QMessageBox_critical';
procedure QMessageBox_about(parent: QWidgetH; title: PWideString; text: PWideString); cdecl; external QtIntf name 'QMessageBox_about';
procedure QMessageBox_aboutQt(parent: QWidgetH; title: PWideString = nil); cdecl; external QtIntf name 'QMessageBox_aboutQt';
procedure QMessageBox_sizeHint(handle: QMessageBoxH; retval: PSize); cdecl; external QtIntf name 'QMessageBox_sizeHint';
function QMessageBox_create(title: PWideString; text: PWideString; icon: QMessageBoxIcon; button0: Integer; button1: Integer; button2: Integer; parent: QWidgetH = nil; f: QtWindowFlags = QtDialog or QtMSWindowsFixedSizeDialogHint): QMessageBoxH; overload; cdecl; external QtIntf name 'QMessageBox_create3';
function QMessageBox_information(parent: QWidgetH; title: PWideString; text: PWideString; button0Text: PWideString; button1Text: PWideString = nil; button2Text: PWideString = nil; defaultButtonNumber: Integer = 0; escapeButtonNumber: Integer = -1): Integer; overload; cdecl; external QtIntf name 'QMessageBox_information3';
function QMessageBox_question(parent: QWidgetH; title: PWideString; text: PWideString; button0Text: PWideString; button1Text: PWideString = nil; button2Text: PWideString = nil; defaultButtonNumber: Integer = 0; escapeButtonNumber: Integer = -1): Integer; overload; cdecl; external QtIntf name 'QMessageBox_question3';
function QMessageBox_warning(parent: QWidgetH; title: PWideString; text: PWideString; button0Text: PWideString; button1Text: PWideString = nil; button2Text: PWideString = nil; defaultButtonNumber: Integer = 0; escapeButtonNumber: Integer = -1): Integer; overload; cdecl; external QtIntf name 'QMessageBox_warning3';
function QMessageBox_critical(parent: QWidgetH; title: PWideString; text: PWideString; button0Text: PWideString; button1Text: PWideString = nil; button2Text: PWideString = nil; defaultButtonNumber: Integer = 0; escapeButtonNumber: Integer = -1): Integer; overload; cdecl; external QtIntf name 'QMessageBox_critical3';
procedure QMessageBox_buttonText(handle: QMessageBoxH; retval: PWideString; button: Integer); cdecl; external QtIntf name 'QMessageBox_buttonText';
procedure QMessageBox_setButtonText(handle: QMessageBoxH; button: Integer; text: PWideString); cdecl; external QtIntf name 'QMessageBox_setButtonText';
procedure QMessageBox_informativeText(handle: QMessageBoxH; retval: PWideString); cdecl; external QtIntf name 'QMessageBox_informativeText';
procedure QMessageBox_setInformativeText(handle: QMessageBoxH; text: PWideString); cdecl; external QtIntf name 'QMessageBox_setInformativeText';
procedure QMessageBox_detailedText(handle: QMessageBoxH; retval: PWideString); cdecl; external QtIntf name 'QMessageBox_detailedText';
procedure QMessageBox_setDetailedText(handle: QMessageBoxH; text: PWideString); cdecl; external QtIntf name 'QMessageBox_setDetailedText';
procedure QMessageBox_setWindowTitle(handle: QMessageBoxH; title: PWideString); cdecl; external QtIntf name 'QMessageBox_setWindowTitle';
procedure QMessageBox_setWindowModality(handle: QMessageBoxH; windowModality: QtWindowModality); cdecl; external QtIntf name 'QMessageBox_setWindowModality';
procedure QMessageBox_standardIcon(retval: QPixmapH; icon: QMessageBoxIcon); cdecl; external QtIntf name 'QMessageBox_standardIcon';

procedure QInputDialog_getText(retval: PWideString; parent: QWidgetH; title: PWideString; _label: PWideString; echo: QLineEditEchoMode = QLineEditNormal; text: PWideString = nil; ok: PBoolean = nil; f: QtWindowFlags = 0); cdecl; external QtIntf name 'QInputDialog_getText';
function QInputDialog_getInteger(parent: QWidgetH; title: PWideString; _label: PWideString; value: Integer = 0; minValue: Integer = -2147483647; maxValue: Integer = 2147483647; step: Integer = 1; ok: PBoolean = nil; f: QtWindowFlags = 0): Integer; cdecl; external QtIntf name 'QInputDialog_getInteger';
function QInputDialog_getDouble(parent: QWidgetH; title: PWideString; _label: PWideString; value: Double = 0; minValue: Double = -2147483647; maxValue: Double = 2147483647; decimals: Integer = 1; ok: PBoolean = nil; f: QtWindowFlags = 0): Double; cdecl; external QtIntf name 'QInputDialog_getDouble';
procedure QInputDialog_getItem(retval: PWideString; parent: QWidgetH; title: PWideString; _label: PWideString; list: QStringListH; current: Integer = 0; editable: Boolean = True; ok: PBoolean = nil; f: QtWindowFlags = 0); cdecl; external QtIntf name 'QInputDialog_getItem';

procedure QColorDialog_getColor(retval: PQColor; init: PQColor; parent: QWidgetH = nil); cdecl; external QtIntf name 'QColorDialog_getColor';
function QColorDialog_getRgba(p1: QRgb; ok: PBoolean = nil; parent: QWidgetH = nil): QRgb; cdecl; external QtIntf name 'QColorDialog_getRgba';
function QColorDialog_customCount(): Integer; cdecl; external QtIntf name 'QColorDialog_customCount';
function QColorDialog_customColor(p1: Integer): QRgb; cdecl; external QtIntf name 'QColorDialog_customColor';
procedure QColorDialog_setCustomColor(p1: Integer; p2: QRgb); cdecl; external QtIntf name 'QColorDialog_setCustomColor';
procedure QColorDialog_setStandardColor(p1: Integer; p2: QRgb); cdecl; external QtIntf name 'QColorDialog_setStandardColor';


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

function QFileDialog_create(parent: QWidgetH; f: QtWindowFlags): QFileDialogH; overload; cdecl; external QtIntf name 'QFileDialog_create';
procedure QFileDialog_destroy(handle: QFileDialogH); cdecl; external QtIntf name 'QFileDialog_destroy'; 
function QFileDialog_create(parent: QWidgetH = nil; caption: PWideString = nil; directory: PWideString = nil; filter: PWideString = nil): QFileDialogH; overload; cdecl; external QtIntf name 'QFileDialog_create2';
procedure QFileDialog_setDirectory(handle: QFileDialogH; directory: PWideString); overload; cdecl; external QtIntf name 'QFileDialog_setDirectory';
procedure QFileDialog_setDirectory(handle: QFileDialogH; directory: QDirH); overload; cdecl; external QtIntf name 'QFileDialog_setDirectory2';
procedure QFileDialog_directory(handle: QFileDialogH; retval: QDirH); cdecl; external QtIntf name 'QFileDialog_directory';
procedure QFileDialog_selectFile(handle: QFileDialogH; filename: PWideString); cdecl; external QtIntf name 'QFileDialog_selectFile';
procedure QFileDialog_selectedFiles(handle: QFileDialogH; retval: QStringListH); cdecl; external QtIntf name 'QFileDialog_selectedFiles';
procedure QFileDialog_setFilter(handle: QFileDialogH; filter: PWideString); cdecl; external QtIntf name 'QFileDialog_setFilter';
procedure QFileDialog_setFilters(handle: QFileDialogH; filters: QStringListH); cdecl; external QtIntf name 'QFileDialog_setFilters';
procedure QFileDialog_filters(handle: QFileDialogH; retval: QStringListH); cdecl; external QtIntf name 'QFileDialog_filters';
procedure QFileDialog_selectFilter(handle: QFileDialogH; filter: PWideString); cdecl; external QtIntf name 'QFileDialog_selectFilter';
procedure QFileDialog_selectedFilter(handle: QFileDialogH; retval: PWideString); cdecl; external QtIntf name 'QFileDialog_selectedFilter';
procedure QFileDialog_setViewMode(handle: QFileDialogH; mode: QFileDialogViewMode); cdecl; external QtIntf name 'QFileDialog_setViewMode';
function QFileDialog_viewMode(handle: QFileDialogH): QFileDialogViewMode; cdecl; external QtIntf name 'QFileDialog_viewMode';
procedure QFileDialog_setFileMode(handle: QFileDialogH; mode: QFileDialogFileMode); cdecl; external QtIntf name 'QFileDialog_setFileMode';
function QFileDialog_fileMode(handle: QFileDialogH): QFileDialogFileMode; cdecl; external QtIntf name 'QFileDialog_fileMode';
procedure QFileDialog_setAcceptMode(handle: QFileDialogH; mode: QFileDialogAcceptMode); cdecl; external QtIntf name 'QFileDialog_setAcceptMode';
function QFileDialog_acceptMode(handle: QFileDialogH): QFileDialogAcceptMode; cdecl; external QtIntf name 'QFileDialog_acceptMode';
procedure QFileDialog_setReadOnly(handle: QFileDialogH; enabled: Boolean); cdecl; external QtIntf name 'QFileDialog_setReadOnly';
function QFileDialog_isReadOnly(handle: QFileDialogH): Boolean; cdecl; external QtIntf name 'QFileDialog_isReadOnly';
procedure QFileDialog_setResolveSymlinks(handle: QFileDialogH; enabled: Boolean); cdecl; external QtIntf name 'QFileDialog_setResolveSymlinks';
function QFileDialog_resolveSymlinks(handle: QFileDialogH): Boolean; cdecl; external QtIntf name 'QFileDialog_resolveSymlinks';
procedure QFileDialog_setConfirmOverwrite(handle: QFileDialogH; enabled: Boolean); cdecl; external QtIntf name 'QFileDialog_setConfirmOverwrite';
function QFileDialog_confirmOverwrite(handle: QFileDialogH): Boolean; cdecl; external QtIntf name 'QFileDialog_confirmOverwrite';
procedure QFileDialog_setDefaultSuffix(handle: QFileDialogH; suffix: PWideString); cdecl; external QtIntf name 'QFileDialog_setDefaultSuffix';
procedure QFileDialog_defaultSuffix(handle: QFileDialogH; retval: PWideString); cdecl; external QtIntf name 'QFileDialog_defaultSuffix';
procedure QFileDialog_setHistory(handle: QFileDialogH; paths: QStringListH); cdecl; external QtIntf name 'QFileDialog_setHistory';
procedure QFileDialog_history(handle: QFileDialogH; retval: QStringListH); cdecl; external QtIntf name 'QFileDialog_history';
procedure QFileDialog_setItemDelegate(handle: QFileDialogH; delegate: QAbstractItemDelegateH); cdecl; external QtIntf name 'QFileDialog_setItemDelegate';
function QFileDialog_itemDelegate(handle: QFileDialogH): QAbstractItemDelegateH; cdecl; external QtIntf name 'QFileDialog_itemDelegate';
procedure QFileDialog_setIconProvider(handle: QFileDialogH; provider: QFileIconProviderH); cdecl; external QtIntf name 'QFileDialog_setIconProvider';
function QFileDialog_iconProvider(handle: QFileDialogH): QFileIconProviderH; cdecl; external QtIntf name 'QFileDialog_iconProvider';
procedure QFileDialog_setLabelText(handle: QFileDialogH; _label: QFileDialogDialogLabel; text: PWideString); cdecl; external QtIntf name 'QFileDialog_setLabelText';
procedure QFileDialog_labelText(handle: QFileDialogH; retval: PWideString; _label: QFileDialogDialogLabel); cdecl; external QtIntf name 'QFileDialog_labelText';
procedure QFileDialog_getOpenFileName(retval: PWideString; parent: QWidgetH = nil; caption: PWideString = nil; dir: PWideString = nil; filter: PWideString = nil; selectedFilter: PWideString = nil; options: QFileDialogOptions = 0); cdecl; external QtIntf name 'QFileDialog_getOpenFileName';
procedure QFileDialog_getSaveFileName(retval: PWideString; parent: QWidgetH = nil; caption: PWideString = nil; dir: PWideString = nil; filter: PWideString = nil; selectedFilter: PWideString = nil; options: QFileDialogOptions = 0); cdecl; external QtIntf name 'QFileDialog_getSaveFileName';
procedure QFileDialog_getExistingDirectory(retval: PWideString; parent: QWidgetH = nil; caption: PWideString = nil; dir: PWideString = nil; options: QFileDialogOptions = QFileDialogShowDirsOnly); cdecl; external QtIntf name 'QFileDialog_getExistingDirectory';
procedure QFileDialog_getOpenFileNames(retval: QStringListH; parent: QWidgetH = nil; caption: PWideString = nil; dir: PWideString = nil; filter: PWideString = nil; selectedFilter: PWideString = nil; options: QFileDialogOptions = 0); cdecl; external QtIntf name 'QFileDialog_getOpenFileNames';


type
  QFileDialog_filesSelected_Event = procedure (files: QStringListH) of object cdecl;
  QFileDialog_currentChanged_Event = procedure (path: PWideString) of object cdecl;


function QProgressDialog_create(parent: QWidgetH = nil; f: QtWindowFlags = 0): QProgressDialogH; overload; cdecl; external QtIntf name 'QProgressDialog_create';
procedure QProgressDialog_destroy(handle: QProgressDialogH); cdecl; external QtIntf name 'QProgressDialog_destroy'; 
function QProgressDialog_create(labelText: PWideString; cancelButtonText: PWideString; minimum: Integer; maximum: Integer; parent: QWidgetH = nil; f: QtWindowFlags = 0): QProgressDialogH; overload; cdecl; external QtIntf name 'QProgressDialog_create2';
procedure QProgressDialog_setLabel(handle: QProgressDialogH; _label: QLabelH); cdecl; external QtIntf name 'QProgressDialog_setLabel';
procedure QProgressDialog_setCancelButton(handle: QProgressDialogH; button: QPushButtonH); cdecl; external QtIntf name 'QProgressDialog_setCancelButton';
procedure QProgressDialog_setBar(handle: QProgressDialogH; bar: QProgressBarH); cdecl; external QtIntf name 'QProgressDialog_setBar';
function QProgressDialog_wasCanceled(handle: QProgressDialogH): Boolean; cdecl; external QtIntf name 'QProgressDialog_wasCanceled';
function QProgressDialog_minimum(handle: QProgressDialogH): Integer; cdecl; external QtIntf name 'QProgressDialog_minimum';
function QProgressDialog_maximum(handle: QProgressDialogH): Integer; cdecl; external QtIntf name 'QProgressDialog_maximum';
procedure QProgressDialog_setRange(handle: QProgressDialogH; minimum: Integer; maximum: Integer); cdecl; external QtIntf name 'QProgressDialog_setRange';
function QProgressDialog_value(handle: QProgressDialogH): Integer; cdecl; external QtIntf name 'QProgressDialog_value';
procedure QProgressDialog_sizeHint(handle: QProgressDialogH; retval: PSize); cdecl; external QtIntf name 'QProgressDialog_sizeHint';
procedure QProgressDialog_labelText(handle: QProgressDialogH; retval: PWideString); cdecl; external QtIntf name 'QProgressDialog_labelText';
function QProgressDialog_minimumDuration(handle: QProgressDialogH): Integer; cdecl; external QtIntf name 'QProgressDialog_minimumDuration';
procedure QProgressDialog_setAutoReset(handle: QProgressDialogH; b: Boolean); cdecl; external QtIntf name 'QProgressDialog_setAutoReset';
function QProgressDialog_autoReset(handle: QProgressDialogH): Boolean; cdecl; external QtIntf name 'QProgressDialog_autoReset';
procedure QProgressDialog_setAutoClose(handle: QProgressDialogH; b: Boolean); cdecl; external QtIntf name 'QProgressDialog_setAutoClose';
function QProgressDialog_autoClose(handle: QProgressDialogH): Boolean; cdecl; external QtIntf name 'QProgressDialog_autoClose';
procedure QProgressDialog_cancel(handle: QProgressDialogH); cdecl; external QtIntf name 'QProgressDialog_cancel';
procedure QProgressDialog_reset(handle: QProgressDialogH); cdecl; external QtIntf name 'QProgressDialog_reset';
procedure QProgressDialog_setMaximum(handle: QProgressDialogH; maximum: Integer); cdecl; external QtIntf name 'QProgressDialog_setMaximum';
procedure QProgressDialog_setMinimum(handle: QProgressDialogH; minimum: Integer); cdecl; external QtIntf name 'QProgressDialog_setMinimum';
procedure QProgressDialog_setValue(handle: QProgressDialogH; progress: Integer); cdecl; external QtIntf name 'QProgressDialog_setValue';
procedure QProgressDialog_setLabelText(handle: QProgressDialogH; p1: PWideString); cdecl; external QtIntf name 'QProgressDialog_setLabelText';
procedure QProgressDialog_setCancelButtonText(handle: QProgressDialogH; p1: PWideString); cdecl; external QtIntf name 'QProgressDialog_setCancelButtonText';
procedure QProgressDialog_setMinimumDuration(handle: QProgressDialogH; ms: Integer); cdecl; external QtIntf name 'QProgressDialog_setMinimumDuration';


type
  QProgressDialog_canceled_Event = procedure () of object cdecl;



type
  QAbstractPrintDialogPrintRange = ( // QAbstractPrintDialog::PrintRange (1)
    QAbstractPrintDialogAllPages, QAbstractPrintDialogSelection, QAbstractPrintDialogPageRange );

type
  QAbstractPrintDialogPrintDialogOption = cardinal; // QAbstractPrintDialog::PrintDialogOption
  QAbstractPrintDialogPrintDialogOptions = QAbstractPrintDialogPrintDialogOption; //QFlags<> (3)
const
  QAbstractPrintDialogNone =   $0000;
  QAbstractPrintDialogPrintToFile =   $0001;
  QAbstractPrintDialogPrintSelection =   $0002;
  QAbstractPrintDialogPrintPageRange =   $0004;
  QAbstractPrintDialogPrintCollateCopies =   $0010;

function QAbstractPrintDialog_exec(handle: QAbstractPrintDialogH): Integer; cdecl; external QtIntf name 'QAbstractPrintDialog_exec';
procedure QAbstractPrintDialog_addEnabledOption(handle: QAbstractPrintDialogH; option: QAbstractPrintDialogPrintDialogOption); cdecl; external QtIntf name 'QAbstractPrintDialog_addEnabledOption';
procedure QAbstractPrintDialog_setEnabledOptions(handle: QAbstractPrintDialogH; options: QAbstractPrintDialogPrintDialogOptions); cdecl; external QtIntf name 'QAbstractPrintDialog_setEnabledOptions';
function QAbstractPrintDialog_enabledOptions(handle: QAbstractPrintDialogH): QAbstractPrintDialogPrintDialogOptions; cdecl; external QtIntf name 'QAbstractPrintDialog_enabledOptions';
function QAbstractPrintDialog_isOptionEnabled(handle: QAbstractPrintDialogH; option: QAbstractPrintDialogPrintDialogOption): Boolean; cdecl; external QtIntf name 'QAbstractPrintDialog_isOptionEnabled';
procedure QAbstractPrintDialog_setPrintRange(handle: QAbstractPrintDialogH; range: QAbstractPrintDialogPrintRange); cdecl; external QtIntf name 'QAbstractPrintDialog_setPrintRange';
function QAbstractPrintDialog_printRange(handle: QAbstractPrintDialogH): QAbstractPrintDialogPrintRange; cdecl; external QtIntf name 'QAbstractPrintDialog_printRange';
procedure QAbstractPrintDialog_setMinMax(handle: QAbstractPrintDialogH; min: Integer; max: Integer); cdecl; external QtIntf name 'QAbstractPrintDialog_setMinMax';
function QAbstractPrintDialog_minPage(handle: QAbstractPrintDialogH): Integer; cdecl; external QtIntf name 'QAbstractPrintDialog_minPage';
function QAbstractPrintDialog_maxPage(handle: QAbstractPrintDialogH): Integer; cdecl; external QtIntf name 'QAbstractPrintDialog_maxPage';
procedure QAbstractPrintDialog_setFromTo(handle: QAbstractPrintDialogH; fromPage: Integer; toPage: Integer); cdecl; external QtIntf name 'QAbstractPrintDialog_setFromTo';
function QAbstractPrintDialog_fromPage(handle: QAbstractPrintDialogH): Integer; cdecl; external QtIntf name 'QAbstractPrintDialog_fromPage';
function QAbstractPrintDialog_toPage(handle: QAbstractPrintDialogH): Integer; cdecl; external QtIntf name 'QAbstractPrintDialog_toPage';
function QAbstractPrintDialog_printer(handle: QAbstractPrintDialogH): QPrinterH; cdecl; external QtIntf name 'QAbstractPrintDialog_printer';

function QPrintDialog_create(printer: QPrinterH; parent: QWidgetH = nil): QPrintDialogH; cdecl; external QtIntf name 'QPrintDialog_create';
procedure QPrintDialog_destroy(handle: QPrintDialogH); cdecl; external QtIntf name 'QPrintDialog_destroy'; 
function QPrintDialog_exec(handle: QPrintDialogH): Integer; cdecl; external QtIntf name 'QPrintDialog_exec';


type
  QSystemTrayIconActivationReason = ( // QSystemTrayIcon::ActivationReason (1)
    QSystemTrayIconUnknown, QSystemTrayIconContext, QSystemTrayIconDoubleClick, QSystemTrayIconTrigger, QSystemTrayIconMiddleClick );

  QSystemTrayIconMessageIcon = ( // QSystemTrayIcon::MessageIcon (1)
    QSystemTrayIconNoIcon, QSystemTrayIconInformation, QSystemTrayIconWarning, QSystemTrayIconCritical );

function QSystemTrayIcon_create(parent: QObjectH = nil): QSystemTrayIconH; overload; cdecl; external QtIntf name 'QSystemTrayIcon_create';
procedure QSystemTrayIcon_destroy(handle: QSystemTrayIconH); cdecl; external QtIntf name 'QSystemTrayIcon_destroy'; 
function QSystemTrayIcon_create(icon: QIconH; parent: QObjectH = nil): QSystemTrayIconH; overload; cdecl; external QtIntf name 'QSystemTrayIcon_create2';
procedure QSystemTrayIcon_setContextMenu(handle: QSystemTrayIconH; menu: QMenuH); cdecl; external QtIntf name 'QSystemTrayIcon_setContextMenu';
function QSystemTrayIcon_contextMenu(handle: QSystemTrayIconH): QMenuH; cdecl; external QtIntf name 'QSystemTrayIcon_contextMenu';
procedure QSystemTrayIcon_icon(handle: QSystemTrayIconH; retval: QIconH); cdecl; external QtIntf name 'QSystemTrayIcon_icon';
procedure QSystemTrayIcon_setIcon(handle: QSystemTrayIconH; icon: QIconH); cdecl; external QtIntf name 'QSystemTrayIcon_setIcon';
procedure QSystemTrayIcon_toolTip(handle: QSystemTrayIconH; retval: PWideString); cdecl; external QtIntf name 'QSystemTrayIcon_toolTip';
procedure QSystemTrayIcon_setToolTip(handle: QSystemTrayIconH; tip: PWideString); cdecl; external QtIntf name 'QSystemTrayIcon_setToolTip';
function QSystemTrayIcon_isSystemTrayAvailable(): Boolean; cdecl; external QtIntf name 'QSystemTrayIcon_isSystemTrayAvailable';
function QSystemTrayIcon_supportsMessages(): Boolean; cdecl; external QtIntf name 'QSystemTrayIcon_supportsMessages';
procedure QSystemTrayIcon_showMessage(handle: QSystemTrayIconH; title: PWideString; msg: PWideString; icon: QSystemTrayIconMessageIcon = QSystemTrayIconInformation; msecs: Integer = 10000); cdecl; external QtIntf name 'QSystemTrayIcon_showMessage';
function QSystemTrayIcon_isVisible(handle: QSystemTrayIconH): Boolean; cdecl; external QtIntf name 'QSystemTrayIcon_isVisible';
procedure QSystemTrayIcon_setVisible(handle: QSystemTrayIconH; visible: Boolean); cdecl; external QtIntf name 'QSystemTrayIcon_setVisible';
procedure QSystemTrayIcon_show(handle: QSystemTrayIconH); cdecl; external QtIntf name 'QSystemTrayIcon_show';
procedure QSystemTrayIcon_hide(handle: QSystemTrayIconH); cdecl; external QtIntf name 'QSystemTrayIcon_hide';


type
  QSystemTrayIcon_activated_Event = procedure (reason: QSystemTrayIconActivationReason) of object cdecl;
  QSystemTrayIcon_messageClicked_Event = procedure () of object cdecl;


type
  QStyleStateFlag = cardinal; // QStyle::StateFlag
  QStyleState = QStyleStateFlag; //QFlags<> (3)
const
  QStyleState_None =   $00000000;
  QStyleState_Enabled =   $00000001;
  QStyleState_Raised =   $00000002;
  QStyleState_Sunken =   $00000004;
  QStyleState_Off =   $00000008;
  QStyleState_NoChange =   $00000010;
  QStyleState_On =   $00000020;
  QStyleState_DownArrow =   $00000040;
  QStyleState_Horizontal =   $00000080;
  QStyleState_HasFocus =   $00000100;
  QStyleState_Top =   $00000200;
  QStyleState_Bottom =   $00000400;
  QStyleState_FocusAtBorder =   $00000800;
  QStyleState_AutoRaise =   $00001000;
  QStyleState_MouseOver =   $00002000;
  QStyleState_UpArrow =   $00004000;
  QStyleState_Selected =   $00008000;
  QStyleState_Active =   $00010000;
  QStyleState_Open =   $00040000;
  QStyleState_Children =   $00080000;
  QStyleState_Item =   $00100000;
  QStyleState_Sibling =   $00200000;
  QStyleState_Editing =   $00400000;
  QStyleState_KeyboardFocusChange =   $00800000;
  QStyleState_ReadOnly =   $02000000;


type
  QStylePrimitiveElement = (  //QStyle::PrimitiveElement (2)
    QStylePE_Q3CheckListController,
    QStylePE_Q3CheckListExclusiveIndicator,
    QStylePE_Q3CheckListIndicator,
    QStylePE_Q3DockWindowSeparator,
    QStylePE_Q3Separator,
    QStylePE_Frame,
    QStylePE_FrameDefaultButton,
    QStylePE_FrameDockWidget,
    QStylePE_FrameFocusRect,
    QStylePE_FrameGroupBox,
    QStylePE_FrameLineEdit,
    QStylePE_FrameMenu,
    QStylePE_FrameStatusBar,
    QStylePE_FrameTabWidget,
    QStylePE_FrameWindow,
    QStylePE_FrameButtonBevel,
    QStylePE_FrameButtonTool,
    QStylePE_FrameTabBarBase,
    QStylePE_PanelButtonCommand,
    QStylePE_PanelButtonBevel,
    QStylePE_PanelButtonTool,
    QStylePE_PanelMenuBar,
    QStylePE_PanelToolBar,
    QStylePE_PanelLineEdit,
    QStylePE_IndicatorArrowDown,
    QStylePE_IndicatorArrowLeft,
    QStylePE_IndicatorArrowRight,
    QStylePE_IndicatorArrowUp,
    QStylePE_IndicatorBranch,
    QStylePE_IndicatorButtonDropDown,
    QStylePE_IndicatorViewItemCheck,
    QStylePE_IndicatorCheckBox,
    QStylePE_IndicatorDockWidgetResizeHandle,
    QStylePE_IndicatorHeaderArrow,
    QStylePE_IndicatorMenuCheckMark,
    QStylePE_IndicatorProgressChunk,
    QStylePE_IndicatorRadioButton,
    QStylePE_IndicatorSpinDown,
    QStylePE_IndicatorSpinMinus,
    QStylePE_IndicatorSpinPlus,
    QStylePE_IndicatorSpinUp,
    QStylePE_IndicatorToolBarHandle,
    QStylePE_IndicatorToolBarSeparator,
    QStylePE_PanelTipLabel,
    QStylePE_IndicatorTabTear,
    QStylePE_PanelScrollAreaCorner,
    QStylePE_Widget,
    QStylePE_CustomBase = $f000000 );

  QStyleControlElement = (  //QStyle::ControlElement (2)
    QStyleCE_PushButton,
    QStyleCE_PushButtonBevel,
    QStyleCE_PushButtonLabel,
    QStyleCE_CheckBox,
    QStyleCE_CheckBoxLabel,
    QStyleCE_RadioButton,
    QStyleCE_RadioButtonLabel,
    QStyleCE_TabBarTab,
    QStyleCE_TabBarTabShape,
    QStyleCE_TabBarTabLabel,
    QStyleCE_ProgressBar,
    QStyleCE_ProgressBarGroove,
    QStyleCE_ProgressBarContents,
    QStyleCE_ProgressBarLabel,
    QStyleCE_MenuItem,
    QStyleCE_MenuScroller,
    QStyleCE_MenuVMargin,
    QStyleCE_MenuHMargin,
    QStyleCE_MenuTearoff,
    QStyleCE_MenuEmptyArea,
    QStyleCE_MenuBarItem,
    QStyleCE_MenuBarEmptyArea,
    QStyleCE_ToolButtonLabel,
    QStyleCE_Header,
    QStyleCE_HeaderSection,
    QStyleCE_HeaderLabel,
    QStyleCE_Q3DockWindowEmptyArea,
    QStyleCE_ToolBoxTab,
    QStyleCE_SizeGrip,
    QStyleCE_Splitter,
    QStyleCE_RubberBand,
    QStyleCE_DockWidgetTitle,
    QStyleCE_ScrollBarAddLine,
    QStyleCE_ScrollBarSubLine,
    QStyleCE_ScrollBarAddPage,
    QStyleCE_ScrollBarSubPage,
    QStyleCE_ScrollBarSlider,
    QStyleCE_ScrollBarFirst,
    QStyleCE_ScrollBarLast,
    QStyleCE_FocusFrame,
    QStyleCE_ComboBoxLabel,
    QStyleCE_ToolBar,
    QStyleCE_CustomBase = $f0000000 );

  QStyleSubElement = (  //QStyle::SubElement (2)
    QStyleSE_PushButtonContents,
    QStyleSE_PushButtonFocusRect,
    QStyleSE_CheckBoxIndicator,
    QStyleSE_CheckBoxContents,
    QStyleSE_CheckBoxFocusRect,
    QStyleSE_CheckBoxClickRect,
    QStyleSE_RadioButtonIndicator,
    QStyleSE_RadioButtonContents,
    QStyleSE_RadioButtonFocusRect,
    QStyleSE_RadioButtonClickRect,
    QStyleSE_ComboBoxFocusRect,
    QStyleSE_SliderFocusRect,
    QStyleSE_Q3DockWindowHandleRect,
    QStyleSE_ProgressBarGroove,
    QStyleSE_ProgressBarContents,
    QStyleSE_ProgressBarLabel,
    QStyleSE_DialogButtonAccept,
    QStyleSE_DialogButtonReject,
    QStyleSE_DialogButtonApply,
    QStyleSE_DialogButtonHelp,
    QStyleSE_DialogButtonAll,
    QStyleSE_DialogButtonAbort,
    QStyleSE_DialogButtonIgnore,
    QStyleSE_DialogButtonRetry,
    QStyleSE_DialogButtonCustom,
    QStyleSE_ToolBoxTabContents,
    QStyleSE_HeaderLabel,
    QStyleSE_HeaderArrow,
    QStyleSE_TabWidgetTabBar,
    QStyleSE_TabWidgetTabPane,
    QStyleSE_TabWidgetTabContents,
    QStyleSE_TabWidgetLeftCorner,
    QStyleSE_TabWidgetRightCorner,
    QStyleSE_ViewItemCheckIndicator,
    QStyleSE_TabBarTearIndicator,
    QStyleSE_TreeViewDisclosureItem,
    QStyleSE_LineEditContents,
    QStyleSE_FrameContents,
    QStyleSE_CustomBase = $f0000000 );

  QStyleComplexControl = (  //QStyle::ComplexControl (2)
    QStyleCC_SpinBox,
    QStyleCC_ComboBox,
    QStyleCC_ScrollBar,
    QStyleCC_Slider,
    QStyleCC_ToolButton,
    QStyleCC_TitleBar,
    QStyleCC_Q3ListView,
    QStyleCC_Dial,
    QStyleCC_GroupBox,
    QStyleCC_CustomBase = $f0000000 );

type
  QStyleSubControl = cardinal; // QStyle::SubControl
  QStyleSubControls = QStyleSubControl; //QFlags<> (3)
const
  QStyleSC_None =   $00000000;
  QStyleSC_ScrollBarAddLine =   $00000001;
  QStyleSC_ScrollBarSubLine =   $00000002;
  QStyleSC_ScrollBarAddPage =   $00000004;
  QStyleSC_ScrollBarSubPage =   $00000008;
  QStyleSC_ScrollBarFirst =   $00000010;
  QStyleSC_ScrollBarLast =   $00000020;
  QStyleSC_ScrollBarSlider =   $00000040;
  QStyleSC_ScrollBarGroove =   $00000080;
  QStyleSC_SpinBoxUp =   $00000001;
  QStyleSC_SpinBoxDown =   $00000002;
  QStyleSC_SpinBoxFrame =   $00000004;
  QStyleSC_SpinBoxEditField =   $00000008;
  QStyleSC_ComboBoxFrame =   $00000001;
  QStyleSC_ComboBoxEditField =   $00000002;
  QStyleSC_ComboBoxArrow =   $00000004;
  QStyleSC_ComboBoxListBoxPopup =   $00000008;
  QStyleSC_SliderGroove =   $00000001;
  QStyleSC_SliderHandle =   $00000002;
  QStyleSC_SliderTickmarks =   $00000004;
  QStyleSC_ToolButton =   $00000001;
  QStyleSC_ToolButtonMenu =   $00000002;
  QStyleSC_TitleBarSysMenu =   $00000001;
  QStyleSC_TitleBarMinButton =   $00000002;
  QStyleSC_TitleBarMaxButton =   $00000004;
  QStyleSC_TitleBarCloseButton =   $00000008;
  QStyleSC_TitleBarNormalButton =   $00000010;
  QStyleSC_TitleBarShadeButton =   $00000020;
  QStyleSC_TitleBarUnshadeButton =   $00000040;
  QStyleSC_TitleBarContextHelpButton =   $00000080;
  QStyleSC_TitleBarLabel =   $00000100;
  QStyleSC_Q3ListView =   $00000001;
  QStyleSC_Q3ListViewBranch =   $00000002;
  QStyleSC_Q3ListViewExpand =   $00000004;
  QStyleSC_DialGroove =   $00000001;
  QStyleSC_DialHandle =   $00000002;
  QStyleSC_DialTickmarks =   $00000004;
  QStyleSC_GroupBoxCheckBox =   $00000001;
  QStyleSC_GroupBoxLabel =   $00000002;
  QStyleSC_GroupBoxContents =   $00000004;
  QStyleSC_GroupBoxFrame =   $00000008;
  QStyleSC_All =   $ffffffff;


type
  QStylePixelMetric = (  //QStyle::PixelMetric (2)
    QStylePM_ButtonMargin,
    QStylePM_ButtonDefaultIndicator,
    QStylePM_MenuButtonIndicator,
    QStylePM_ButtonShiftHorizontal,
    QStylePM_ButtonShiftVertical,
    QStylePM_DefaultFrameWidth,
    QStylePM_SpinBoxFrameWidth,
    QStylePM_ComboBoxFrameWidth,
    QStylePM_MaximumDragDistance,
    QStylePM_ScrollBarExtent,
    QStylePM_ScrollBarSliderMin,
    QStylePM_SliderThickness,
    QStylePM_SliderControlThickness,
    QStylePM_SliderLength,
    QStylePM_SliderTickmarkOffset,
    QStylePM_SliderSpaceAvailable,
    QStylePM_DockWidgetSeparatorExtent,
    QStylePM_DockWidgetHandleExtent,
    QStylePM_DockWidgetFrameWidth,
    QStylePM_TabBarTabOverlap,
    QStylePM_TabBarTabHSpace,
    QStylePM_TabBarTabVSpace,
    QStylePM_TabBarBaseHeight,
    QStylePM_TabBarBaseOverlap,
    QStylePM_ProgressBarChunkWidth,
    QStylePM_SplitterWidth,
    QStylePM_TitleBarHeight,
    QStylePM_MenuScrollerHeight,
    QStylePM_MenuHMargin,
    QStylePM_MenuVMargin,
    QStylePM_MenuPanelWidth,
    QStylePM_MenuTearoffHeight,
    QStylePM_MenuDesktopFrameWidth,
    QStylePM_MenuBarPanelWidth,
    QStylePM_MenuBarItemSpacing,
    QStylePM_MenuBarVMargin,
    QStylePM_MenuBarHMargin,
    QStylePM_IndicatorWidth,
    QStylePM_IndicatorHeight,
    QStylePM_ExclusiveIndicatorWidth,
    QStylePM_ExclusiveIndicatorHeight,
    QStylePM_CheckListButtonSize,
    QStylePM_CheckListControllerSize,
    QStylePM_DialogButtonsSeparator,
    QStylePM_DialogButtonsButtonWidth,
    QStylePM_DialogButtonsButtonHeight,
    QStylePM_MDIFrameWidth,
    QStylePM_MDIMinimizedWidth,
    QStylePM_HeaderMargin,
    QStylePM_HeaderMarkSize,
    QStylePM_HeaderGripMargin,
    QStylePM_TabBarTabShiftHorizontal,
    QStylePM_TabBarTabShiftVertical,
    QStylePM_TabBarScrollButtonWidth,
    QStylePM_ToolBarFrameWidth,
    QStylePM_ToolBarHandleExtent,
    QStylePM_ToolBarItemSpacing,
    QStylePM_ToolBarItemMargin,
    QStylePM_ToolBarSeparatorExtent,
    QStylePM_ToolBarExtensionExtent,
    QStylePM_SpinBoxSliderHeight,
    QStylePM_DefaultTopLevelMargin,
    QStylePM_DefaultChildMargin,
    QStylePM_DefaultLayoutSpacing,
    QStylePM_ToolBarIconSize,
    QStylePM_ListViewIconSize,
    QStylePM_IconViewIconSize,
    QStylePM_SmallIconSize,
    QStylePM_LargeIconSize,
    QStylePM_FocusFrameVMargin,
    QStylePM_FocusFrameHMargin,
    QStylePM_ToolTipLabelFrameWidth,
    QStylePM_CheckBoxLabelSpacing,
    QStylePM_TabBarIconSize,
    QStylePM_SizeGripSize,
    QStylePM_DockWidgetTitleMargin,
    QStylePM_MessageBoxIconSize,
    QStylePM_ButtonIconSize,
    QStylePM_CustomBase = $f0000000 );

  QStyleContentsType = (  //QStyle::ContentsType (2)
    QStyleCT_PushButton,
    QStyleCT_CheckBox,
    QStyleCT_RadioButton,
    QStyleCT_ToolButton,
    QStyleCT_ComboBox,
    QStyleCT_Splitter,
    QStyleCT_Q3DockWindow,
    QStyleCT_ProgressBar,
    QStyleCT_MenuItem,
    QStyleCT_MenuBarItem,
    QStyleCT_MenuBar,
    QStyleCT_Menu,
    QStyleCT_TabBarTab,
    QStyleCT_Slider,
    QStyleCT_ScrollBar,
    QStyleCT_Q3Header,
    QStyleCT_LineEdit,
    QStyleCT_SpinBox,
    QStyleCT_SizeGrip,
    QStyleCT_TabWidget,
    QStyleCT_DialogButtons,
    QStyleCT_HeaderSection,
    QStyleCT_GroupBox,
    QStyleCT_CustomBase = $f0000000 );

  QStyleStandardPixmap = (  //QStyle::StandardPixmap (2)
    QStyleSP_TitleBarMenuButton,
    QStyleSP_TitleBarMinButton,
    QStyleSP_TitleBarMaxButton,
    QStyleSP_TitleBarCloseButton,
    QStyleSP_TitleBarNormalButton,
    QStyleSP_TitleBarShadeButton,
    QStyleSP_TitleBarUnshadeButton,
    QStyleSP_TitleBarContextHelpButton,
    QStyleSP_DockWidgetCloseButton,
    QStyleSP_MessageBoxInformation,
    QStyleSP_MessageBoxWarning,
    QStyleSP_MessageBoxCritical,
    QStyleSP_MessageBoxQuestion,
    QStyleSP_DesktopIcon,
    QStyleSP_TrashIcon,
    QStyleSP_ComputerIcon,
    QStyleSP_DriveFDIcon,
    QStyleSP_DriveHDIcon,
    QStyleSP_DriveCDIcon,
    QStyleSP_DriveDVDIcon,
    QStyleSP_DriveNetIcon,
    QStyleSP_DirOpenIcon,
    QStyleSP_DirClosedIcon,
    QStyleSP_DirLinkIcon,
    QStyleSP_FileIcon,
    QStyleSP_FileLinkIcon,
    QStyleSP_ToolBarHorizontalExtensionButton,
    QStyleSP_ToolBarVerticalExtensionButton,
    QStyleSP_FileDialogStart,
    QStyleSP_FileDialogEnd,
    QStyleSP_FileDialogToParent,
    QStyleSP_FileDialogNewFolder,
    QStyleSP_FileDialogDetailedView,
    QStyleSP_FileDialogInfoView,
    QStyleSP_FileDialogContentsView,
    QStyleSP_FileDialogListView,
    QStyleSP_FileDialogBack,
    QStyleSP_DirIcon,
    QStyleSP_DialogOkButton,
    QStyleSP_DialogCancelButton,
    QStyleSP_DialogHelpButton,
    QStyleSP_DialogOpenButton,
    QStyleSP_DialogSaveButton,
    QStyleSP_DialogCloseButton,
    QStyleSP_DialogApplyButton,
    QStyleSP_DialogResetButton,
    QStyleSP_DialogDiscardButton,
    QStyleSP_DialogYesButton,
    QStyleSP_DialogNoButton,
    QStyleSP_ArrowUp,
    QStyleSP_ArrowDown,
    QStyleSP_ArrowLeft,
    QStyleSP_ArrowRight,
    QStyleSP_ArrowBack,
    QStyleSP_ArrowForward,
    QStyleSP_CustomBase = $f0000000 );

type
  QStyleStyleHint = cardinal; //  QStyle::StyleHint (4)

const
    QStyleSH_EtchDisabledText = 0 { $0 };
    QStyleSH_DitherDisabledText = 1 { $1 };
    QStyleSH_ScrollBar_MiddleClickAbsolutePosition = 2 { $2 };
    QStyleSH_ScrollBar_ScrollWhenPointerLeavesControl = 3 { $3 };
    QStyleSH_TabBar_SelectMouseType = 4 { $4 };
    QStyleSH_TabBar_Alignment = 5 { $5 };
    QStyleSH_Header_ArrowAlignment = 6 { $6 };
    QStyleSH_Slider_SnapToValue = 7 { $7 };
    QStyleSH_Slider_SloppyKeyEvents = 8 { $8 };
    QStyleSH_ProgressDialog_CenterCancelButton = 9 { $9 };
    QStyleSH_ProgressDialog_TextLabelAlignment = 10 { $a };
    QStyleSH_PrintDialog_RightAlignButtons = 11 { $b };
    QStyleSH_MainWindow_SpaceBelowMenuBar = 12 { $c };
    QStyleSH_FontDialog_SelectAssociatedText = 13 { $d };
    QStyleSH_Menu_AllowActiveAndDisabled = 14 { $e };
    QStyleSH_Menu_SpaceActivatesItem = 15 { $f };
    QStyleSH_Menu_SubMenuPopupDelay = 16 { $10 };
    QStyleSH_ScrollView_FrameOnlyAroundContents = 17 { $11 };
    QStyleSH_MenuBar_AltKeyNavigation = 18 { $12 };
    QStyleSH_ComboBox_ListMouseTracking = 19 { $13 };
    QStyleSH_Menu_MouseTracking = 20 { $14 };
    QStyleSH_MenuBar_MouseTracking = 21 { $15 };
    QStyleSH_ItemView_ChangeHighlightOnFocus = 22 { $16 };
    QStyleSH_Widget_ShareActivation = 23 { $17 };
    QStyleSH_Workspace_FillSpaceOnMaximize = 24 { $18 };
    QStyleSH_ComboBox_Popup = 25 { $19 };
    QStyleSH_TitleBar_NoBorder = 26 { $1a };
    QStyleSH_Slider_StopMouseOverSlider = 27 { $1b };
    QStyleSH_ScrollBar_StopMouseOverSlider = 27 { $1b };
    QStyleSH_BlinkCursorWhenTextSelected = 28 { $1c };
    QStyleSH_RichText_FullWidthSelection = 29 { $1d };
    QStyleSH_Menu_Scrollable = 30 { $1e };
    QStyleSH_GroupBox_TextLabelVerticalAlignment = 31 { $1f };
    QStyleSH_GroupBox_TextLabelColor = 32 { $20 };
    QStyleSH_Menu_SloppySubMenus = 33 { $21 };
    QStyleSH_Table_GridLineColor = 34 { $22 };
    QStyleSH_LineEdit_PasswordCharacter = 35 { $23 };
    QStyleSH_DialogButtons_DefaultButton = 36 { $24 };
    QStyleSH_ToolBox_SelectedPageTitleBold = 37 { $25 };
    QStyleSH_TabBar_PreferNoArrows = 38 { $26 };
    QStyleSH_ScrollBar_LeftClickAbsolutePosition = 39 { $27 };
    QStyleSH_Q3ListViewExpand_SelectMouseType = 40 { $28 };
    QStyleSH_UnderlineShortcut = 41 { $29 };
    QStyleSH_SpinBox_AnimateButton = 42 { $2a };
    QStyleSH_SpinBox_KeyPressAutoRepeatRate = 43 { $2b };
    QStyleSH_SpinBox_ClickAutoRepeatRate = 44 { $2c };
    QStyleSH_Menu_FillScreenWithScroll = 45 { $2d };
    QStyleSH_ToolTipLabel_Opacity = 46 { $2e };
    QStyleSH_DrawMenuBarSeparator = 47 { $2f };
    QStyleSH_TitleBar_ModifyNotification = 48 { $30 };
    QStyleSH_Button_FocusPolicy = 49 { $31 };
    QStyleSH_MenuBar_DismissOnSecondClick = 50 { $32 };
    QStyleSH_MessageBox_UseBorderForButtonSpacing = 51 { $33 };
    QStyleSH_TitleBar_AutoRaise = 52 { $34 };
    QStyleSH_ToolButton_PopupDelay = 53 { $35 };
    QStyleSH_FocusFrame_Mask = 54 { $36 };
    QStyleSH_RubberBand_Mask = 55 { $37 };
    QStyleSH_WindowFrame_Mask = 56 { $38 };
    QStyleSH_SpinControls_DisableOnBounds = 57 { $39 };
    QStyleSH_Dial_BackgroundRole = 58 { $3a };
    QStyleSH_ComboBox_LayoutDirection = 59 { $3b };
    QStyleSH_ItemView_EllipsisLocation = 60 { $3c };
    QStyleSH_ItemView_ShowDecorationSelected = 61 { $3d };
    QStyleSH_ItemView_ActivateItemOnSingleClick = 62 { $3e };
    QStyleSH_ScrollBar_ContextMenu = 63 { $3f };
    QStyleSH_ScrollBar_RollBetweenButtons = 64 { $40 };
    QStyleSH_Slider_AbsoluteSetButtons = 65 { $41 };
    QStyleSH_Slider_PageSetButtons = 66 { $42 };
    QStyleSH_Menu_KeyboardSearch = 67 { $43 };
    QStyleSH_TabBar_ElideMode = 68 { $44 };
    QStyleSH_DialogButtonLayout = 69 { $45 };
    QStyleSH_ComboBox_PopupFrameStyle = 70 { $46 };
    QStyleSH_MessageBox_TextInteractionFlags = 71 { $47 };
    QStyleSH_DialogButtonBox_ButtonsHaveIcons = 72 { $48 };
    QStyleSH_SpellCheckUnderlineStyle = 73 { $49 };
    QStyleSH_MessageBox_CenterButtons = 74 { $4a };
    QStyleSH_Menu_SelectionWrap = 75 { $4b };
    QStyleSH_ItemView_MovementWithoutUpdatingSelection = 76 { $4c };
    QStyleSH_CustomBase = 4026531840 { $f0000000 };


procedure QStyle_polish(handle: QStyleH; p1: QWidgetH); overload; cdecl; external QtIntf name 'QStyle_polish';
procedure QStyle_unpolish(handle: QStyleH; p1: QWidgetH); overload; cdecl; external QtIntf name 'QStyle_unpolish';
procedure QStyle_polish(handle: QStyleH; p1: QApplicationH); overload; cdecl; external QtIntf name 'QStyle_polish2';
procedure QStyle_unpolish(handle: QStyleH; p1: QApplicationH); overload; cdecl; external QtIntf name 'QStyle_unpolish2';
procedure QStyle_polish(handle: QStyleH; p1: QPaletteH); overload; cdecl; external QtIntf name 'QStyle_polish3';
procedure QStyle_itemTextRect(handle: QStyleH; retval: PRect; fm: QFontMetricsH; r: PRect; flags: Integer; enabled: Boolean; text: PWideString); cdecl; external QtIntf name 'QStyle_itemTextRect';
procedure QStyle_itemPixmapRect(handle: QStyleH; retval: PRect; r: PRect; flags: Integer; pixmap: QPixmapH); cdecl; external QtIntf name 'QStyle_itemPixmapRect';
procedure QStyle_drawItemText(handle: QStyleH; painter: QPainterH; rect: PRect; flags: Integer; pal: QPaletteH; enabled: Boolean; text: PWideString; textRole: QPaletteColorRole = QPaletteNoRole); cdecl; external QtIntf name 'QStyle_drawItemText';
procedure QStyle_drawItemPixmap(handle: QStyleH; painter: QPainterH; rect: PRect; alignment: Integer; pixmap: QPixmapH); cdecl; external QtIntf name 'QStyle_drawItemPixmap';
procedure QStyle_standardPalette(handle: QStyleH; retval: QPaletteH); cdecl; external QtIntf name 'QStyle_standardPalette';
procedure QStyle_drawPrimitive(handle: QStyleH; pe: QStylePrimitiveElement; opt: QStyleOptionH; p: QPainterH; w: QWidgetH = nil); cdecl; external QtIntf name 'QStyle_drawPrimitive';
procedure QStyle_drawControl(handle: QStyleH; element: QStyleControlElement; opt: QStyleOptionH; p: QPainterH; w: QWidgetH = nil); cdecl; external QtIntf name 'QStyle_drawControl';
procedure QStyle_subElementRect(handle: QStyleH; retval: PRect; subElement: QStyleSubElement; option: QStyleOptionH; widget: QWidgetH = nil); cdecl; external QtIntf name 'QStyle_subElementRect';
procedure QStyle_drawComplexControl(handle: QStyleH; cc: QStyleComplexControl; opt: QStyleOptionComplexH; p: QPainterH; widget: QWidgetH = nil); cdecl; external QtIntf name 'QStyle_drawComplexControl';
function QStyle_hitTestComplexControl(handle: QStyleH; cc: QStyleComplexControl; opt: QStyleOptionComplexH; pt: PQtPoint; widget: QWidgetH = nil): QStyleSubControl; cdecl; external QtIntf name 'QStyle_hitTestComplexControl';
procedure QStyle_subControlRect(handle: QStyleH; retval: PRect; cc: QStyleComplexControl; opt: QStyleOptionComplexH; sc: QStyleSubControl; widget: QWidgetH = nil); cdecl; external QtIntf name 'QStyle_subControlRect';
function QStyle_pixelMetric(handle: QStyleH; metric: QStylePixelMetric; option: QStyleOptionH = nil; widget: QWidgetH = nil): Integer; cdecl; external QtIntf name 'QStyle_pixelMetric';
procedure QStyle_sizeFromContents(handle: QStyleH; retval: PSize; ct: QStyleContentsType; opt: QStyleOptionH; contentsSize: PSize; w: QWidgetH = nil); cdecl; external QtIntf name 'QStyle_sizeFromContents';
function QStyle_styleHint(handle: QStyleH; stylehint: QStyleStyleHint; opt: QStyleOptionH = nil; widget: QWidgetH = nil; returnData: QStyleHintReturnH = nil): Integer; cdecl; external QtIntf name 'QStyle_styleHint';
procedure QStyle_standardPixmap(handle: QStyleH; retval: QPixmapH; standardPixmap: QStyleStandardPixmap; opt: QStyleOptionH = nil; widget: QWidgetH = nil); cdecl; external QtIntf name 'QStyle_standardPixmap';
procedure QStyle_standardIcon(handle: QStyleH; retval: QIconH; standardIcon: QStyleStandardPixmap; option: QStyleOptionH = nil; widget: QWidgetH = nil); cdecl; external QtIntf name 'QStyle_standardIcon';
procedure QStyle_generatedIconPixmap(handle: QStyleH; retval: QPixmapH; iconMode: QIconMode; pixmap: QPixmapH; opt: QStyleOptionH); cdecl; external QtIntf name 'QStyle_generatedIconPixmap';
procedure QStyle_visualRect(retval: PRect; direction: QtLayoutDirection; boundingRect: PRect; logicalRect: PRect); cdecl; external QtIntf name 'QStyle_visualRect';
procedure QStyle_visualPos(retval: PQtPoint; direction: QtLayoutDirection; boundingRect: PRect; logicalPos: PQtPoint); cdecl; external QtIntf name 'QStyle_visualPos';
function QStyle_sliderPositionFromValue(min: Integer; max: Integer; val: Integer; space: Integer; upsideDown: Boolean = False): Integer; cdecl; external QtIntf name 'QStyle_sliderPositionFromValue';
function QStyle_sliderValueFromPosition(min: Integer; max: Integer; pos: Integer; space: Integer; upsideDown: Boolean = False): Integer; cdecl; external QtIntf name 'QStyle_sliderValueFromPosition';
function QStyle_visualAlignment(direction: QtLayoutDirection; alignment: QtAlignment): QtAlignment; cdecl; external QtIntf name 'QStyle_visualAlignment';
procedure QStyle_alignedRect(retval: PRect; direction: QtLayoutDirection; alignment: QtAlignment; size: PSize; rectangle: PRect); cdecl; external QtIntf name 'QStyle_alignedRect';


type
  QStyleOptionOptionType = (  //QStyleOption::OptionType (2)
    QStyleOptionSO_Default,
    QStyleOptionSO_FocusRect,
    QStyleOptionSO_Button,
    QStyleOptionSO_Tab,
    QStyleOptionSO_MenuItem,
    QStyleOptionSO_Frame,
    QStyleOptionSO_ProgressBar,
    QStyleOptionSO_ToolBox,
    QStyleOptionSO_Header,
    QStyleOptionSO_Q3DockWindow,
    QStyleOptionSO_DockWidget,
    QStyleOptionSO_Q3ListViewItem,
    QStyleOptionSO_ViewItem,
    QStyleOptionSO_TabWidgetFrame,
    QStyleOptionSO_TabBarBase,
    QStyleOptionSO_RubberBand,
    QStyleOptionSO_ToolBar,
    QStyleOptionSO_GraphicsItem,
    QStyleOptionSO_Complex = $f0000,
    QStyleOptionSO_Slider,
    QStyleOptionSO_SpinBox,
    QStyleOptionSO_ToolButton,
    QStyleOptionSO_ComboBox,
    QStyleOptionSO_Q3ListView,
    QStyleOptionSO_TitleBar,
    QStyleOptionSO_GroupBox,
    QStyleOptionSO_SizeGrip,
    QStyleOptionSO_CustomBase = $f00,
    QStyleOptionSO_ComplexCustomBase = $f000000 );

  QStyleOptionStyleOptionVersion = (  //QStyleOption::StyleOptionVersion (2s)
    QStyleOptionVersion = 1 );


type
  QStyleOptionFocusRectStyleOptionVersion = (  //QStyleOptionFocusRect::StyleOptionVersion (2s)
    QStyleOptionFocusRectVersion = 1 );


type
  QStyleOptionFrameStyleOptionVersion = (  //QStyleOptionFrame::StyleOptionVersion (2s)
    QStyleOptionFrameVersion = 1 );


type
  QStyleOptionFrameV2StyleOptionVersion = (  //QStyleOptionFrameV2::StyleOptionVersion (2s)
    QStyleOptionFrameV2Version = 2 );

type
  QStyleOptionFrameV2FrameFeature = cardinal; // QStyleOptionFrameV2::FrameFeature
  QStyleOptionFrameV2FrameFeatures = QStyleOptionFrameV2FrameFeature; //QFlags<> (3)
const
  QStyleOptionFrameV2None =   $00;
  QStyleOptionFrameV2Flat =   $01;


type
  QStyleOptionTabWidgetFrameStyleOptionVersion = (  //QStyleOptionTabWidgetFrame::StyleOptionVersion (2s)
    QStyleOptionTabWidgetFrameVersion = 1 );


type
  QStyleOptionTabBarBaseStyleOptionVersion = (  //QStyleOptionTabBarBase::StyleOptionVersion (2s)
    QStyleOptionTabBarBaseVersion = 1 );


type
  QStyleOptionHeaderSectionPosition = ( // QStyleOptionHeader::SectionPosition (1)
    QStyleOptionHeaderBeginning, QStyleOptionHeaderMiddle, QStyleOptionHeaderEnd, QStyleOptionHeaderOnlyOneSection );

  QStyleOptionHeaderSelectedPosition = ( // QStyleOptionHeader::SelectedPosition (1)
    QStyleOptionHeaderNotAdjacent, QStyleOptionHeaderNextIsSelected, QStyleOptionHeaderPreviousIsSelected, QStyleOptionHeaderNextAndPreviousAreSelected );

  QStyleOptionHeaderSortIndicator = ( // QStyleOptionHeader::SortIndicator (1)
    QStyleOptionHeaderNone, QStyleOptionHeaderSortUp, QStyleOptionHeaderSortDown );

  QStyleOptionHeaderStyleOptionVersion = (  //QStyleOptionHeader::StyleOptionVersion (2s)
    QStyleOptionHeaderVersion = 1 );


type
  QStyleOptionButtonStyleOptionVersion = (  //QStyleOptionButton::StyleOptionVersion (2s)
    QStyleOptionButtonVersion = 1 );

type
  QStyleOptionButtonButtonFeature = cardinal; // QStyleOptionButton::ButtonFeature
  QStyleOptionButtonButtonFeatures = QStyleOptionButtonButtonFeature; //QFlags<> (3)
const
  QStyleOptionButtonNone =   $00;
  QStyleOptionButtonFlat =   $01;
  QStyleOptionButtonHasMenu =   $02;
  QStyleOptionButtonDefaultButton =   $04;
  QStyleOptionButtonAutoDefaultButton =   $08;


type
  QStyleOptionTabTabPosition = ( // QStyleOptionTab::TabPosition (1)
    QStyleOptionTabBeginning, QStyleOptionTabMiddle, QStyleOptionTabEnd, QStyleOptionTabOnlyOneTab );

  QStyleOptionTabSelectedPosition = ( // QStyleOptionTab::SelectedPosition (1)
    QStyleOptionTabNotAdjacent, QStyleOptionTabNextIsSelected, QStyleOptionTabPreviousIsSelected );

  QStyleOptionTabStyleOptionVersion = (  //QStyleOptionTab::StyleOptionVersion (2s)
    QStyleOptionTabVersion = 1 );

type
  QStyleOptionTabCornerWidget = cardinal; // QStyleOptionTab::CornerWidget
  QStyleOptionTabCornerWidgets = QStyleOptionTabCornerWidget; //QFlags<> (3)
const
  QStyleOptionTabNoCornerWidgets =   $00;
  QStyleOptionTabLeftCornerWidget =   $01;
  QStyleOptionTabRightCornerWidget =   $02;


type
  QStyleOptionTabV2StyleOptionVersion = (  //QStyleOptionTabV2::StyleOptionVersion (2s)
    QStyleOptionTabV2Version = 2 );


type
  QStyleOptionToolBarToolBarPosition = ( // QStyleOptionToolBar::ToolBarPosition (1)
    QStyleOptionToolBarBeginning, QStyleOptionToolBarMiddle, QStyleOptionToolBarEnd, QStyleOptionToolBarOnlyOne );

  QStyleOptionToolBarStyleOptionVersion = (  //QStyleOptionToolBar::StyleOptionVersion (2s)
    QStyleOptionToolBarVersion = 1 );

type
  QStyleOptionToolBarToolBarFeature = cardinal; // QStyleOptionToolBar::ToolBarFeature
  QStyleOptionToolBarToolBarFeatures = QStyleOptionToolBarToolBarFeature; //QFlags<> (3)
const
  QStyleOptionToolBarNone =   $0;
  QStyleOptionToolBarMovable =   $1;


type
  QStyleOptionProgressBarStyleOptionVersion = (  //QStyleOptionProgressBar::StyleOptionVersion (2s)
    QStyleOptionProgressBarVersion = 1 );


type
  QStyleOptionProgressBarV2StyleOptionVersion = (  //QStyleOptionProgressBarV2::StyleOptionVersion (2s)
    QStyleOptionProgressBarV2Version = 2 );


type
  QStyleOptionMenuItemMenuItemType = ( // QStyleOptionMenuItem::MenuItemType (1)
    QStyleOptionMenuItemNormal, QStyleOptionMenuItemDefaultItem, QStyleOptionMenuItemSeparator, QStyleOptionMenuItemSubMenu, QStyleOptionMenuItemScroller, QStyleOptionMenuItemTearOff, QStyleOptionMenuItemMargin, 
    QStyleOptionMenuItemEmptyArea );

  QStyleOptionMenuItemCheckType = ( // QStyleOptionMenuItem::CheckType (1)
    QStyleOptionMenuItemNotCheckable, QStyleOptionMenuItemExclusive, QStyleOptionMenuItemNonExclusive );

  QStyleOptionMenuItemStyleOptionVersion = (  //QStyleOptionMenuItem::StyleOptionVersion (2s)
    QStyleOptionMenuItemVersion = 1 );


type
  QStyleOptionQ3ListViewItemStyleOptionVersion = (  //QStyleOptionQ3ListViewItem::StyleOptionVersion (2s)
    QStyleOptionQ3ListViewItemVersion = 1 );

type
  QStyleOptionQ3ListViewItemQ3ListViewItemFeature = cardinal; // QStyleOptionQ3ListViewItem::Q3ListViewItemFeature
  QStyleOptionQ3ListViewItemQ3ListViewItemFeatures = QStyleOptionQ3ListViewItemQ3ListViewItemFeature; //QFlags<> (3)
const
  QStyleOptionQ3ListViewItemNone =   $00;
  QStyleOptionQ3ListViewItemExpandable =   $01;
  QStyleOptionQ3ListViewItemMultiLine =   $02;
  QStyleOptionQ3ListViewItemVisible =   $04;
  QStyleOptionQ3ListViewItemParentControl =   $08;


type
  QStyleOptionQ3DockWindowStyleOptionVersion = (  //QStyleOptionQ3DockWindow::StyleOptionVersion (2s)
    QStyleOptionQ3DockWindowVersion = 1 );


type
  QStyleOptionDockWidgetStyleOptionVersion = (  //QStyleOptionDockWidget::StyleOptionVersion (2s)
    QStyleOptionDockWidgetVersion = 1 );


type
  QStyleOptionViewItemPosition = ( // QStyleOptionViewItem::Position (1)
    QStyleOptionViewItemLeft, QStyleOptionViewItemRight, QStyleOptionViewItemTop, QStyleOptionViewItemBottom );

  QStyleOptionViewItemStyleOptionVersion = (  //QStyleOptionViewItem::StyleOptionVersion (2s)
    QStyleOptionViewItemVersion = 1 );


type
  QStyleOptionViewItemV2StyleOptionVersion = (  //QStyleOptionViewItemV2::StyleOptionVersion (2s)
    QStyleOptionViewItemV2Version = 2 );

type
  QStyleOptionViewItemV2ViewItemFeature = cardinal; // QStyleOptionViewItemV2::ViewItemFeature
  QStyleOptionViewItemV2ViewItemFeatures = QStyleOptionViewItemV2ViewItemFeature; //QFlags<> (3)
const
  QStyleOptionViewItemV2None =   $00;
  QStyleOptionViewItemV2WrapText =   $01;
  QStyleOptionViewItemV2Alternate =   $02;


type
  QStyleOptionToolBoxStyleOptionVersion = (  //QStyleOptionToolBox::StyleOptionVersion (2s)
    QStyleOptionToolBoxVersion = 1 );


type
  QStyleOptionRubberBandStyleOptionVersion = (  //QStyleOptionRubberBand::StyleOptionVersion (2s)
    QStyleOptionRubberBandVersion = 1 );


type
  QStyleOptionComplexStyleOptionVersion = (  //QStyleOptionComplex::StyleOptionVersion (2s)
    QStyleOptionComplexVersion = 1 );


type
  QStyleOptionSliderStyleOptionVersion = (  //QStyleOptionSlider::StyleOptionVersion (2s)
    QStyleOptionSliderVersion = 1 );


type
  QStyleOptionSpinBoxStyleOptionVersion = (  //QStyleOptionSpinBox::StyleOptionVersion (2s)
    QStyleOptionSpinBoxVersion = 1 );


type
  QStyleOptionQ3ListViewStyleOptionVersion = (  //QStyleOptionQ3ListView::StyleOptionVersion (2s)
    QStyleOptionQ3ListViewVersion = 1 );


type
  QStyleOptionToolButtonStyleOptionVersion = (  //QStyleOptionToolButton::StyleOptionVersion (2s)
    QStyleOptionToolButtonVersion = 1 );

type
  QStyleOptionToolButtonToolButtonFeature = cardinal; // QStyleOptionToolButton::ToolButtonFeature
  QStyleOptionToolButtonToolButtonFeatures = QStyleOptionToolButtonToolButtonFeature; //QFlags<> (3)
const
  QStyleOptionToolButtonNone =   $00;
  QStyleOptionToolButtonArrow =   $01;
  QStyleOptionToolButtonMenu =   $04;
  QStyleOptionToolButtonPopupDelay =   $08;


type
  QStyleOptionComboBoxStyleOptionVersion = (  //QStyleOptionComboBox::StyleOptionVersion (2s)
    QStyleOptionComboBoxVersion = 1 );


type
  QStyleOptionTitleBarStyleOptionVersion = (  //QStyleOptionTitleBar::StyleOptionVersion (2s)
    QStyleOptionTitleBarVersion = 1 );


type
  QStyleOptionGroupBoxStyleOptionVersion = (  //QStyleOptionGroupBox::StyleOptionVersion (2s)
    QStyleOptionGroupBoxVersion = 1 );


type
  QStyleOptionSizeGripStyleOptionVersion = (  //QStyleOptionSizeGrip::StyleOptionVersion (2s)
    QStyleOptionSizeGripVersion = 1 );


type
  QStyleOptionGraphicsItemStyleOptionVersion = (  //QStyleOptionGraphicsItem::StyleOptionVersion (2s)
    QStyleOptionGraphicsItemVersion = 1 );


type
  QStyleHintReturnHintReturnType = (  //QStyleHintReturn::HintReturnType (2)
    QStyleHintReturnSH_Default = $f000,
    QStyleHintReturnSH_Mask );

  QStyleHintReturnStyleOptionVersion = (  //QStyleHintReturn::StyleOptionVersion (2s)
    QStyleHintReturnVersion = 1 );


type
  QStyleHintReturnMaskStyleOptionVersion = (  //QStyleHintReturnMask::StyleOptionVersion (2s)
    QStyleHintReturnMaskVersion = 1 );

type
  QStyleOptionStyleOptionType = cardinal; //  QStyleOption::StyleOptionType (4)

const
    QStyleOptionType = 0 { $0 };

type
  QStyleOptionFocusRectStyleOptionType = cardinal; //  QStyleOptionFocusRect::StyleOptionType (4)

const
    QStyleOptionFocusRectType = 1 { $1 };

type
  QStyleOptionFrameStyleOptionType = cardinal; //  QStyleOptionFrame::StyleOptionType (4)

const
    QStyleOptionFrameType = 5 { $5 };

type
  QStyleOptionTabWidgetFrameStyleOptionType = cardinal; //  QStyleOptionTabWidgetFrame::StyleOptionType (4)

const
    QStyleOptionTabWidgetFrameType = 13 { $d };

type
  QStyleOptionTabBarBaseStyleOptionType = cardinal; //  QStyleOptionTabBarBase::StyleOptionType (4)

const
    QStyleOptionTabBarBaseType = 14 { $e };

type
  QStyleOptionHeaderStyleOptionType = cardinal; //  QStyleOptionHeader::StyleOptionType (4)

const
    QStyleOptionHeaderType = 8 { $8 };

type
  QStyleOptionButtonStyleOptionType = cardinal; //  QStyleOptionButton::StyleOptionType (4)

const
    QStyleOptionButtonType = 2 { $2 };

type
  QStyleOptionTabStyleOptionType = cardinal; //  QStyleOptionTab::StyleOptionType (4)

const
    QStyleOptionTabType = 3 { $3 };

type
  QStyleOptionToolBarStyleOptionType = cardinal; //  QStyleOptionToolBar::StyleOptionType (4)

const
    QStyleOptionToolBarType = 16 { $10 };

type
  QStyleOptionProgressBarStyleOptionType = cardinal; //  QStyleOptionProgressBar::StyleOptionType (4)

const
    QStyleOptionProgressBarType = 6 { $6 };

type
  QStyleOptionProgressBarV2StyleOptionType = cardinal; //  QStyleOptionProgressBarV2::StyleOptionType (4)

const
    QStyleOptionProgressBarV2Type = 6 { $6 };

type
  QStyleOptionMenuItemStyleOptionType = cardinal; //  QStyleOptionMenuItem::StyleOptionType (4)

const
    QStyleOptionMenuItemType = 4 { $4 };

type
  QStyleOptionQ3ListViewItemStyleOptionType = cardinal; //  QStyleOptionQ3ListViewItem::StyleOptionType (4)

const
    QStyleOptionQ3ListViewItemType = 11 { $b };

type
  QStyleOptionQ3DockWindowStyleOptionType = cardinal; //  QStyleOptionQ3DockWindow::StyleOptionType (4)

const
    QStyleOptionQ3DockWindowType = 9 { $9 };

type
  QStyleOptionDockWidgetStyleOptionType = cardinal; //  QStyleOptionDockWidget::StyleOptionType (4)

const
    QStyleOptionDockWidgetType = 10 { $a };

type
  QStyleOptionViewItemStyleOptionType = cardinal; //  QStyleOptionViewItem::StyleOptionType (4)

const
    QStyleOptionViewItemType = 12 { $c };

type
  QStyleOptionToolBoxStyleOptionType = cardinal; //  QStyleOptionToolBox::StyleOptionType (4)

const
    QStyleOptionToolBoxType = 7 { $7 };

type
  QStyleOptionRubberBandStyleOptionType = cardinal; //  QStyleOptionRubberBand::StyleOptionType (4)

const
    QStyleOptionRubberBandType = 15 { $f };

type
  QStyleOptionComplexStyleOptionType = cardinal; //  QStyleOptionComplex::StyleOptionType (4)

const
    QStyleOptionComplexType = 983040 { $f0000 };

type
  QStyleOptionSliderStyleOptionType = cardinal; //  QStyleOptionSlider::StyleOptionType (4)

const
    QStyleOptionSliderType = 983041 { $f0001 };

type
  QStyleOptionSpinBoxStyleOptionType = cardinal; //  QStyleOptionSpinBox::StyleOptionType (4)

const
    QStyleOptionSpinBoxType = 983042 { $f0002 };

type
  QStyleOptionQ3ListViewStyleOptionType = cardinal; //  QStyleOptionQ3ListView::StyleOptionType (4)

const
    QStyleOptionQ3ListViewType = 983045 { $f0005 };

type
  QStyleOptionToolButtonStyleOptionType = cardinal; //  QStyleOptionToolButton::StyleOptionType (4)

const
    QStyleOptionToolButtonType = 983043 { $f0003 };

type
  QStyleOptionComboBoxStyleOptionType = cardinal; //  QStyleOptionComboBox::StyleOptionType (4)

const
    QStyleOptionComboBoxType = 983044 { $f0004 };

type
  QStyleOptionTitleBarStyleOptionType = cardinal; //  QStyleOptionTitleBar::StyleOptionType (4)

const
    QStyleOptionTitleBarType = 983046 { $f0006 };

type
  QStyleOptionGroupBoxStyleOptionType = cardinal; //  QStyleOptionGroupBox::StyleOptionType (4)

const
    QStyleOptionGroupBoxType = 983047 { $f0007 };

type
  QStyleOptionSizeGripStyleOptionType = cardinal; //  QStyleOptionSizeGrip::StyleOptionType (4)

const
    QStyleOptionSizeGripType = 983048 { $f0008 };

type
  QStyleOptionGraphicsItemStyleOptionType = cardinal; //  QStyleOptionGraphicsItem::StyleOptionType (4)

const
    QStyleOptionGraphicsItemType = 17 { $11 };

type
  QStyleHintReturnStyleOptionType = cardinal; //  QStyleHintReturn::StyleOptionType (4)

const
    QStyleHintReturnType = 61440 { $f000 };

type
  QStyleHintReturnMaskStyleOptionType = cardinal; //  QStyleHintReturnMask::StyleOptionType (4)

const
    QStyleHintReturnMaskType = 61441 { $f001 };


function QStyleOption_version(handle : QStyleOptionH) : Integer; cdecl; external QtIntf name 'QStyleOption_version';
procedure QStyleOption_setVersion(handle : QStyleOptionH; version : Integer); cdecl; external QtIntf name 'QStyleOption_setVersion';
function QStyleOption__type(handle : QStyleOptionH) : Integer; cdecl; external QtIntf name 'QStyleOption__type';
procedure QStyleOption_setType(handle : QStyleOptionH; _type : Integer); cdecl; external QtIntf name 'QStyleOption_setType';
function QStyleOption_state(handle : QStyleOptionH) : QStyleState; cdecl; external QtIntf name 'QStyleOption_state';
procedure QStyleOption_setState(handle : QStyleOptionH; state : QStyleState); cdecl; external QtIntf name 'QStyleOption_setState';
function QStyleOption_direction(handle : QStyleOptionH) : QtLayoutDirection; cdecl; external QtIntf name 'QStyleOption_direction';
procedure QStyleOption_setDirection(handle : QStyleOptionH; direction : QtLayoutDirection); cdecl; external QtIntf name 'QStyleOption_setDirection';
procedure QStyleOption_rect(handle : QStyleOptionH; retval : PRect ); cdecl; external QtIntf name 'QStyleOption_rect';
procedure QStyleOption_setRect(handle : QStyleOptionH; rect :  PRect); cdecl; external QtIntf name 'QStyleOption_setRect';
procedure QStyleOption_fontMetrics(handle : QStyleOptionH; retval : QFontMetricsH ); cdecl; external QtIntf name 'QStyleOption_fontMetrics';
procedure QStyleOption_setFontMetrics(handle : QStyleOptionH; fontMetrics :  QFontMetricsH); cdecl; external QtIntf name 'QStyleOption_setFontMetrics';
procedure QStyleOption_palette(handle : QStyleOptionH; retval : QPaletteH ); cdecl; external QtIntf name 'QStyleOption_palette';
procedure QStyleOption_setPalette(handle : QStyleOptionH; palette :  QPaletteH); cdecl; external QtIntf name 'QStyleOption_setPalette';
function QStyleOption_create(version: Integer = QStyleOptionVersion; _type: Integer = QStyleOptionSO_Default): QStyleOptionH; overload; cdecl; external QtIntf name 'QStyleOption_create';
procedure QStyleOption_destroy(handle: QStyleOptionH); cdecl; external QtIntf name 'QStyleOption_destroy'; 
function QStyleOption_create(other: QStyleOptionH): QStyleOptionH; overload; cdecl; external QtIntf name 'QStyleOption_create2';
procedure QStyleOption_init(handle: QStyleOptionH; w: QWidgetH); cdecl; external QtIntf name 'QStyleOption_init';
procedure QStyleOption_initFrom(handle: QStyleOptionH; w: QWidgetH); cdecl; external QtIntf name 'QStyleOption_initFrom';

procedure QStyleOptionFocusRect_backgroundColor(handle : QStyleOptionFocusRectH; retval : PQColor ); cdecl; external QtIntf name 'QStyleOptionFocusRect_backgroundColor';
procedure QStyleOptionFocusRect_setBackgroundColor(handle : QStyleOptionFocusRectH; backgroundColor :  PQColor); cdecl; external QtIntf name 'QStyleOptionFocusRect_setBackgroundColor';
function QStyleOptionFocusRect_create(): QStyleOptionFocusRectH; overload; cdecl; external QtIntf name 'QStyleOptionFocusRect_create';
procedure QStyleOptionFocusRect_destroy(handle: QStyleOptionFocusRectH); cdecl; external QtIntf name 'QStyleOptionFocusRect_destroy'; 
function QStyleOptionFocusRect_create(other: QStyleOptionFocusRectH): QStyleOptionFocusRectH; overload; cdecl; external QtIntf name 'QStyleOptionFocusRect_create2';

function QStyleOptionFrame_lineWidth(handle : QStyleOptionFrameH) : Integer; cdecl; external QtIntf name 'QStyleOptionFrame_lineWidth';
procedure QStyleOptionFrame_setLineWidth(handle : QStyleOptionFrameH; lineWidth : Integer); cdecl; external QtIntf name 'QStyleOptionFrame_setLineWidth';
function QStyleOptionFrame_midLineWidth(handle : QStyleOptionFrameH) : Integer; cdecl; external QtIntf name 'QStyleOptionFrame_midLineWidth';
procedure QStyleOptionFrame_setMidLineWidth(handle : QStyleOptionFrameH; midLineWidth : Integer); cdecl; external QtIntf name 'QStyleOptionFrame_setMidLineWidth';
function QStyleOptionFrame_create(): QStyleOptionFrameH; overload; cdecl; external QtIntf name 'QStyleOptionFrame_create';
procedure QStyleOptionFrame_destroy(handle: QStyleOptionFrameH); cdecl; external QtIntf name 'QStyleOptionFrame_destroy'; 
function QStyleOptionFrame_create(other: QStyleOptionFrameH): QStyleOptionFrameH; overload; cdecl; external QtIntf name 'QStyleOptionFrame_create2';

function QStyleOptionFrameV2_features(handle : QStyleOptionFrameV2H) : QStyleOptionFrameV2FrameFeatures; cdecl; external QtIntf name 'QStyleOptionFrameV2_features';
procedure QStyleOptionFrameV2_setFeatures(handle : QStyleOptionFrameV2H; features : QStyleOptionFrameV2FrameFeatures); cdecl; external QtIntf name 'QStyleOptionFrameV2_setFeatures';
function QStyleOptionFrameV2_create(): QStyleOptionFrameV2H; overload; cdecl; external QtIntf name 'QStyleOptionFrameV2_create';
procedure QStyleOptionFrameV2_destroy(handle: QStyleOptionFrameV2H); cdecl; external QtIntf name 'QStyleOptionFrameV2_destroy'; 
function QStyleOptionFrameV2_create(other: QStyleOptionFrameV2H): QStyleOptionFrameV2H; overload; cdecl; external QtIntf name 'QStyleOptionFrameV2_create2';
function QStyleOptionFrameV2_create(other: QStyleOptionFrameH): QStyleOptionFrameV2H; overload; cdecl; external QtIntf name 'QStyleOptionFrameV2_create3';

function QStyleOptionTabWidgetFrame_lineWidth(handle : QStyleOptionTabWidgetFrameH) : Integer; cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_lineWidth';
procedure QStyleOptionTabWidgetFrame_setLineWidth(handle : QStyleOptionTabWidgetFrameH; lineWidth : Integer); cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_setLineWidth';
function QStyleOptionTabWidgetFrame_midLineWidth(handle : QStyleOptionTabWidgetFrameH) : Integer; cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_midLineWidth';
procedure QStyleOptionTabWidgetFrame_setMidLineWidth(handle : QStyleOptionTabWidgetFrameH; midLineWidth : Integer); cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_setMidLineWidth';
function QStyleOptionTabWidgetFrame_shape(handle : QStyleOptionTabWidgetFrameH) : QTabBarShape; cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_shape';
procedure QStyleOptionTabWidgetFrame_setShape(handle : QStyleOptionTabWidgetFrameH; shape : QTabBarShape); cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_setShape';
procedure QStyleOptionTabWidgetFrame_tabBarSize(handle : QStyleOptionTabWidgetFrameH; retval : PSize ); cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_tabBarSize';
procedure QStyleOptionTabWidgetFrame_setTabBarSize(handle : QStyleOptionTabWidgetFrameH; tabBarSize :  PSize); cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_setTabBarSize';
procedure QStyleOptionTabWidgetFrame_rightCornerWidgetSize(handle : QStyleOptionTabWidgetFrameH; retval : PSize ); cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_rightCornerWidgetSize';
procedure QStyleOptionTabWidgetFrame_setRightCornerWidgetSize(handle : QStyleOptionTabWidgetFrameH; rightCornerWidgetSize :  PSize); cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_setRightCornerWidgetSize';
procedure QStyleOptionTabWidgetFrame_leftCornerWidgetSize(handle : QStyleOptionTabWidgetFrameH; retval : PSize ); cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_leftCornerWidgetSize';
procedure QStyleOptionTabWidgetFrame_setLeftCornerWidgetSize(handle : QStyleOptionTabWidgetFrameH; leftCornerWidgetSize :  PSize); cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_setLeftCornerWidgetSize';
function QStyleOptionTabWidgetFrame_create(): QStyleOptionTabWidgetFrameH; overload; cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_create';
procedure QStyleOptionTabWidgetFrame_destroy(handle: QStyleOptionTabWidgetFrameH); cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_destroy'; 
function QStyleOptionTabWidgetFrame_create(other: QStyleOptionTabWidgetFrameH): QStyleOptionTabWidgetFrameH; overload; cdecl; external QtIntf name 'QStyleOptionTabWidgetFrame_create2';

function QStyleOptionTabBarBase_shape(handle : QStyleOptionTabBarBaseH) : QTabBarShape; cdecl; external QtIntf name 'QStyleOptionTabBarBase_shape';
procedure QStyleOptionTabBarBase_setShape(handle : QStyleOptionTabBarBaseH; shape : QTabBarShape); cdecl; external QtIntf name 'QStyleOptionTabBarBase_setShape';
procedure QStyleOptionTabBarBase_tabBarRect(handle : QStyleOptionTabBarBaseH; retval : PRect ); cdecl; external QtIntf name 'QStyleOptionTabBarBase_tabBarRect';
procedure QStyleOptionTabBarBase_setTabBarRect(handle : QStyleOptionTabBarBaseH; tabBarRect :  PRect); cdecl; external QtIntf name 'QStyleOptionTabBarBase_setTabBarRect';
procedure QStyleOptionTabBarBase_selectedTabRect(handle : QStyleOptionTabBarBaseH; retval : PRect ); cdecl; external QtIntf name 'QStyleOptionTabBarBase_selectedTabRect';
procedure QStyleOptionTabBarBase_setSelectedTabRect(handle : QStyleOptionTabBarBaseH; selectedTabRect :  PRect); cdecl; external QtIntf name 'QStyleOptionTabBarBase_setSelectedTabRect';
function QStyleOptionTabBarBase_create(): QStyleOptionTabBarBaseH; overload; cdecl; external QtIntf name 'QStyleOptionTabBarBase_create';
procedure QStyleOptionTabBarBase_destroy(handle: QStyleOptionTabBarBaseH); cdecl; external QtIntf name 'QStyleOptionTabBarBase_destroy'; 
function QStyleOptionTabBarBase_create(other: QStyleOptionTabBarBaseH): QStyleOptionTabBarBaseH; overload; cdecl; external QtIntf name 'QStyleOptionTabBarBase_create2';

function QStyleOptionHeader_section(handle : QStyleOptionHeaderH) : Integer; cdecl; external QtIntf name 'QStyleOptionHeader_section';
procedure QStyleOptionHeader_setSection(handle : QStyleOptionHeaderH; section : Integer); cdecl; external QtIntf name 'QStyleOptionHeader_setSection';
procedure QStyleOptionHeader_text(handle : QStyleOptionHeaderH; retval : PWideString ); cdecl; external QtIntf name 'QStyleOptionHeader_text';
procedure QStyleOptionHeader_setText(handle : QStyleOptionHeaderH; text :  PWideString); cdecl; external QtIntf name 'QStyleOptionHeader_setText';
function QStyleOptionHeader_textAlignment(handle : QStyleOptionHeaderH) : QtAlignment; cdecl; external QtIntf name 'QStyleOptionHeader_textAlignment';
procedure QStyleOptionHeader_setTextAlignment(handle : QStyleOptionHeaderH; textAlignment : QtAlignment); cdecl; external QtIntf name 'QStyleOptionHeader_setTextAlignment';
procedure QStyleOptionHeader_icon(handle : QStyleOptionHeaderH; retval : QIconH ); cdecl; external QtIntf name 'QStyleOptionHeader_icon';
procedure QStyleOptionHeader_setIcon(handle : QStyleOptionHeaderH; icon :  QIconH); cdecl; external QtIntf name 'QStyleOptionHeader_setIcon';
function QStyleOptionHeader_iconAlignment(handle : QStyleOptionHeaderH) : QtAlignment; cdecl; external QtIntf name 'QStyleOptionHeader_iconAlignment';
procedure QStyleOptionHeader_setIconAlignment(handle : QStyleOptionHeaderH; iconAlignment : QtAlignment); cdecl; external QtIntf name 'QStyleOptionHeader_setIconAlignment';
function QStyleOptionHeader_position(handle : QStyleOptionHeaderH) : QStyleOptionHeaderSectionPosition; cdecl; external QtIntf name 'QStyleOptionHeader_position';
procedure QStyleOptionHeader_setPosition(handle : QStyleOptionHeaderH; position : QStyleOptionHeaderSectionPosition); cdecl; external QtIntf name 'QStyleOptionHeader_setPosition';
function QStyleOptionHeader_selectedPosition(handle : QStyleOptionHeaderH) : QStyleOptionHeaderSelectedPosition; cdecl; external QtIntf name 'QStyleOptionHeader_selectedPosition';
procedure QStyleOptionHeader_setSelectedPosition(handle : QStyleOptionHeaderH; selectedPosition : QStyleOptionHeaderSelectedPosition); cdecl; external QtIntf name 'QStyleOptionHeader_setSelectedPosition';
function QStyleOptionHeader_sortIndicator(handle : QStyleOptionHeaderH) : QStyleOptionHeaderSortIndicator; cdecl; external QtIntf name 'QStyleOptionHeader_sortIndicator';
procedure QStyleOptionHeader_setSortIndicator(handle : QStyleOptionHeaderH; sortIndicator : QStyleOptionHeaderSortIndicator); cdecl; external QtIntf name 'QStyleOptionHeader_setSortIndicator';
function QStyleOptionHeader_orientation(handle : QStyleOptionHeaderH) : QtOrientation; cdecl; external QtIntf name 'QStyleOptionHeader_orientation';
procedure QStyleOptionHeader_setOrientation(handle : QStyleOptionHeaderH; orientation : QtOrientation); cdecl; external QtIntf name 'QStyleOptionHeader_setOrientation';
function QStyleOptionHeader_create(): QStyleOptionHeaderH; overload; cdecl; external QtIntf name 'QStyleOptionHeader_create';
procedure QStyleOptionHeader_destroy(handle: QStyleOptionHeaderH); cdecl; external QtIntf name 'QStyleOptionHeader_destroy'; 
function QStyleOptionHeader_create(other: QStyleOptionHeaderH): QStyleOptionHeaderH; overload; cdecl; external QtIntf name 'QStyleOptionHeader_create2';

function QStyleOptionButton_features(handle : QStyleOptionButtonH) : QStyleOptionButtonButtonFeatures; cdecl; external QtIntf name 'QStyleOptionButton_features';
procedure QStyleOptionButton_setFeatures(handle : QStyleOptionButtonH; features : QStyleOptionButtonButtonFeatures); cdecl; external QtIntf name 'QStyleOptionButton_setFeatures';
procedure QStyleOptionButton_text(handle : QStyleOptionButtonH; retval : PWideString ); cdecl; external QtIntf name 'QStyleOptionButton_text';
procedure QStyleOptionButton_setText(handle : QStyleOptionButtonH; text :  PWideString); cdecl; external QtIntf name 'QStyleOptionButton_setText';
procedure QStyleOptionButton_icon(handle : QStyleOptionButtonH; retval : QIconH ); cdecl; external QtIntf name 'QStyleOptionButton_icon';
procedure QStyleOptionButton_setIcon(handle : QStyleOptionButtonH; icon :  QIconH); cdecl; external QtIntf name 'QStyleOptionButton_setIcon';
procedure QStyleOptionButton_iconSize(handle : QStyleOptionButtonH; retval : PSize ); cdecl; external QtIntf name 'QStyleOptionButton_iconSize';
procedure QStyleOptionButton_setIconSize(handle : QStyleOptionButtonH; iconSize :  PSize); cdecl; external QtIntf name 'QStyleOptionButton_setIconSize';
function QStyleOptionButton_create(): QStyleOptionButtonH; overload; cdecl; external QtIntf name 'QStyleOptionButton_create';
procedure QStyleOptionButton_destroy(handle: QStyleOptionButtonH); cdecl; external QtIntf name 'QStyleOptionButton_destroy'; 
function QStyleOptionButton_create(other: QStyleOptionButtonH): QStyleOptionButtonH; overload; cdecl; external QtIntf name 'QStyleOptionButton_create2';

function QStyleOptionTab_shape(handle : QStyleOptionTabH) : QTabBarShape; cdecl; external QtIntf name 'QStyleOptionTab_shape';
procedure QStyleOptionTab_setShape(handle : QStyleOptionTabH; shape : QTabBarShape); cdecl; external QtIntf name 'QStyleOptionTab_setShape';
procedure QStyleOptionTab_text(handle : QStyleOptionTabH; retval : PWideString ); cdecl; external QtIntf name 'QStyleOptionTab_text';
procedure QStyleOptionTab_setText(handle : QStyleOptionTabH; text :  PWideString); cdecl; external QtIntf name 'QStyleOptionTab_setText';
procedure QStyleOptionTab_icon(handle : QStyleOptionTabH; retval : QIconH ); cdecl; external QtIntf name 'QStyleOptionTab_icon';
procedure QStyleOptionTab_setIcon(handle : QStyleOptionTabH; icon :  QIconH); cdecl; external QtIntf name 'QStyleOptionTab_setIcon';
function QStyleOptionTab_row(handle : QStyleOptionTabH) : Integer; cdecl; external QtIntf name 'QStyleOptionTab_row';
procedure QStyleOptionTab_setRow(handle : QStyleOptionTabH; row : Integer); cdecl; external QtIntf name 'QStyleOptionTab_setRow';
function QStyleOptionTab_position(handle : QStyleOptionTabH) : QStyleOptionTabTabPosition; cdecl; external QtIntf name 'QStyleOptionTab_position';
procedure QStyleOptionTab_setPosition(handle : QStyleOptionTabH; position : QStyleOptionTabTabPosition); cdecl; external QtIntf name 'QStyleOptionTab_setPosition';
function QStyleOptionTab_selectedPosition(handle : QStyleOptionTabH) : QStyleOptionTabSelectedPosition; cdecl; external QtIntf name 'QStyleOptionTab_selectedPosition';
procedure QStyleOptionTab_setSelectedPosition(handle : QStyleOptionTabH; selectedPosition : QStyleOptionTabSelectedPosition); cdecl; external QtIntf name 'QStyleOptionTab_setSelectedPosition';
function QStyleOptionTab_cornerWidgets(handle : QStyleOptionTabH) : QStyleOptionTabCornerWidgets; cdecl; external QtIntf name 'QStyleOptionTab_cornerWidgets';
procedure QStyleOptionTab_setCornerWidgets(handle : QStyleOptionTabH; cornerWidgets : QStyleOptionTabCornerWidgets); cdecl; external QtIntf name 'QStyleOptionTab_setCornerWidgets';
function QStyleOptionTab_create(): QStyleOptionTabH; overload; cdecl; external QtIntf name 'QStyleOptionTab_create';
procedure QStyleOptionTab_destroy(handle: QStyleOptionTabH); cdecl; external QtIntf name 'QStyleOptionTab_destroy'; 
function QStyleOptionTab_create(other: QStyleOptionTabH): QStyleOptionTabH; overload; cdecl; external QtIntf name 'QStyleOptionTab_create2';

procedure QStyleOptionTabV2_iconSize(handle : QStyleOptionTabV2H; retval : PSize ); cdecl; external QtIntf name 'QStyleOptionTabV2_iconSize';
procedure QStyleOptionTabV2_setIconSize(handle : QStyleOptionTabV2H; iconSize :  PSize); cdecl; external QtIntf name 'QStyleOptionTabV2_setIconSize';
function QStyleOptionTabV2_create(): QStyleOptionTabV2H; overload; cdecl; external QtIntf name 'QStyleOptionTabV2_create';
procedure QStyleOptionTabV2_destroy(handle: QStyleOptionTabV2H); cdecl; external QtIntf name 'QStyleOptionTabV2_destroy'; 
function QStyleOptionTabV2_create(other: QStyleOptionTabV2H): QStyleOptionTabV2H; overload; cdecl; external QtIntf name 'QStyleOptionTabV2_create2';
function QStyleOptionTabV2_create(other: QStyleOptionTabH): QStyleOptionTabV2H; overload; cdecl; external QtIntf name 'QStyleOptionTabV2_create3';

function QStyleOptionToolBar_positionOfLine(handle : QStyleOptionToolBarH) : QStyleOptionToolBarToolBarPosition; cdecl; external QtIntf name 'QStyleOptionToolBar_positionOfLine';
procedure QStyleOptionToolBar_setPositionOfLine(handle : QStyleOptionToolBarH; positionOfLine : QStyleOptionToolBarToolBarPosition); cdecl; external QtIntf name 'QStyleOptionToolBar_setPositionOfLine';
function QStyleOptionToolBar_positionWithinLine(handle : QStyleOptionToolBarH) : QStyleOptionToolBarToolBarPosition; cdecl; external QtIntf name 'QStyleOptionToolBar_positionWithinLine';
procedure QStyleOptionToolBar_setPositionWithinLine(handle : QStyleOptionToolBarH; positionWithinLine : QStyleOptionToolBarToolBarPosition); cdecl; external QtIntf name 'QStyleOptionToolBar_setPositionWithinLine';
function QStyleOptionToolBar_toolBarArea(handle : QStyleOptionToolBarH) : QtToolBarArea; cdecl; external QtIntf name 'QStyleOptionToolBar_toolBarArea';
procedure QStyleOptionToolBar_setToolBarArea(handle : QStyleOptionToolBarH; toolBarArea : QtToolBarArea); cdecl; external QtIntf name 'QStyleOptionToolBar_setToolBarArea';
function QStyleOptionToolBar_features(handle : QStyleOptionToolBarH) : QStyleOptionToolBarToolBarFeatures; cdecl; external QtIntf name 'QStyleOptionToolBar_features';
procedure QStyleOptionToolBar_setFeatures(handle : QStyleOptionToolBarH; features : QStyleOptionToolBarToolBarFeatures); cdecl; external QtIntf name 'QStyleOptionToolBar_setFeatures';
function QStyleOptionToolBar_lineWidth(handle : QStyleOptionToolBarH) : Integer; cdecl; external QtIntf name 'QStyleOptionToolBar_lineWidth';
procedure QStyleOptionToolBar_setLineWidth(handle : QStyleOptionToolBarH; lineWidth : Integer); cdecl; external QtIntf name 'QStyleOptionToolBar_setLineWidth';
function QStyleOptionToolBar_midLineWidth(handle : QStyleOptionToolBarH) : Integer; cdecl; external QtIntf name 'QStyleOptionToolBar_midLineWidth';
procedure QStyleOptionToolBar_setMidLineWidth(handle : QStyleOptionToolBarH; midLineWidth : Integer); cdecl; external QtIntf name 'QStyleOptionToolBar_setMidLineWidth';
function QStyleOptionToolBar_create(): QStyleOptionToolBarH; overload; cdecl; external QtIntf name 'QStyleOptionToolBar_create';
procedure QStyleOptionToolBar_destroy(handle: QStyleOptionToolBarH); cdecl; external QtIntf name 'QStyleOptionToolBar_destroy'; 
function QStyleOptionToolBar_create(other: QStyleOptionToolBarH): QStyleOptionToolBarH; overload; cdecl; external QtIntf name 'QStyleOptionToolBar_create2';

function QStyleOptionProgressBar_minimum(handle : QStyleOptionProgressBarH) : Integer; cdecl; external QtIntf name 'QStyleOptionProgressBar_minimum';
procedure QStyleOptionProgressBar_setMinimum(handle : QStyleOptionProgressBarH; minimum : Integer); cdecl; external QtIntf name 'QStyleOptionProgressBar_setMinimum';
function QStyleOptionProgressBar_maximum(handle : QStyleOptionProgressBarH) : Integer; cdecl; external QtIntf name 'QStyleOptionProgressBar_maximum';
procedure QStyleOptionProgressBar_setMaximum(handle : QStyleOptionProgressBarH; maximum : Integer); cdecl; external QtIntf name 'QStyleOptionProgressBar_setMaximum';
function QStyleOptionProgressBar_progress(handle : QStyleOptionProgressBarH) : Integer; cdecl; external QtIntf name 'QStyleOptionProgressBar_progress';
procedure QStyleOptionProgressBar_setProgress(handle : QStyleOptionProgressBarH; progress : Integer); cdecl; external QtIntf name 'QStyleOptionProgressBar_setProgress';
procedure QStyleOptionProgressBar_text(handle : QStyleOptionProgressBarH; retval : PWideString ); cdecl; external QtIntf name 'QStyleOptionProgressBar_text';
procedure QStyleOptionProgressBar_setText(handle : QStyleOptionProgressBarH; text :  PWideString); cdecl; external QtIntf name 'QStyleOptionProgressBar_setText';
function QStyleOptionProgressBar_textAlignment(handle : QStyleOptionProgressBarH) : QtAlignment; cdecl; external QtIntf name 'QStyleOptionProgressBar_textAlignment';
procedure QStyleOptionProgressBar_setTextAlignment(handle : QStyleOptionProgressBarH; textAlignment : QtAlignment); cdecl; external QtIntf name 'QStyleOptionProgressBar_setTextAlignment';
function QStyleOptionProgressBar_textVisible(handle : QStyleOptionProgressBarH) : Boolean; cdecl; external QtIntf name 'QStyleOptionProgressBar_textVisible';
procedure QStyleOptionProgressBar_setTextVisible(handle : QStyleOptionProgressBarH; textVisible : Boolean); cdecl; external QtIntf name 'QStyleOptionProgressBar_setTextVisible';
function QStyleOptionProgressBar_create(): QStyleOptionProgressBarH; overload; cdecl; external QtIntf name 'QStyleOptionProgressBar_create';
procedure QStyleOptionProgressBar_destroy(handle: QStyleOptionProgressBarH); cdecl; external QtIntf name 'QStyleOptionProgressBar_destroy'; 
function QStyleOptionProgressBar_create(other: QStyleOptionProgressBarH): QStyleOptionProgressBarH; overload; cdecl; external QtIntf name 'QStyleOptionProgressBar_create2';

function QStyleOptionProgressBarV2_orientation(handle : QStyleOptionProgressBarV2H) : QtOrientation; cdecl; external QtIntf name 'QStyleOptionProgressBarV2_orientation';
procedure QStyleOptionProgressBarV2_setOrientation(handle : QStyleOptionProgressBarV2H; orientation : QtOrientation); cdecl; external QtIntf name 'QStyleOptionProgressBarV2_setOrientation';
function QStyleOptionProgressBarV2_invertedAppearance(handle : QStyleOptionProgressBarV2H) : Boolean; cdecl; external QtIntf name 'QStyleOptionProgressBarV2_invertedAppearance';
procedure QStyleOptionProgressBarV2_setInvertedAppearance(handle : QStyleOptionProgressBarV2H; invertedAppearance : Boolean); cdecl; external QtIntf name 'QStyleOptionProgressBarV2_setInvertedAppearance';
function QStyleOptionProgressBarV2_bottomToTop(handle : QStyleOptionProgressBarV2H) : Boolean; cdecl; external QtIntf name 'QStyleOptionProgressBarV2_bottomToTop';
procedure QStyleOptionProgressBarV2_setBottomToTop(handle : QStyleOptionProgressBarV2H; bottomToTop : Boolean); cdecl; external QtIntf name 'QStyleOptionProgressBarV2_setBottomToTop';
function QStyleOptionProgressBarV2_create(): QStyleOptionProgressBarV2H; overload; cdecl; external QtIntf name 'QStyleOptionProgressBarV2_create';
procedure QStyleOptionProgressBarV2_destroy(handle: QStyleOptionProgressBarV2H); cdecl; external QtIntf name 'QStyleOptionProgressBarV2_destroy'; 
function QStyleOptionProgressBarV2_create(other: QStyleOptionProgressBarH): QStyleOptionProgressBarV2H; overload; cdecl; external QtIntf name 'QStyleOptionProgressBarV2_create2';
function QStyleOptionProgressBarV2_create(other: QStyleOptionProgressBarV2H): QStyleOptionProgressBarV2H; overload; cdecl; external QtIntf name 'QStyleOptionProgressBarV2_create3';

function QStyleOptionMenuItem_menuItemType(handle : QStyleOptionMenuItemH) : QStyleOptionMenuItemMenuItemType; cdecl; external QtIntf name 'QStyleOptionMenuItem_menuItemType';
procedure QStyleOptionMenuItem_setMenuItemType(handle : QStyleOptionMenuItemH; menuItemType : QStyleOptionMenuItemMenuItemType); cdecl; external QtIntf name 'QStyleOptionMenuItem_setMenuItemType';
function QStyleOptionMenuItem_checkType(handle : QStyleOptionMenuItemH) : QStyleOptionMenuItemCheckType; cdecl; external QtIntf name 'QStyleOptionMenuItem_checkType';
procedure QStyleOptionMenuItem_setCheckType(handle : QStyleOptionMenuItemH; checkType : QStyleOptionMenuItemCheckType); cdecl; external QtIntf name 'QStyleOptionMenuItem_setCheckType';
function QStyleOptionMenuItem_checked(handle : QStyleOptionMenuItemH) : Boolean; cdecl; external QtIntf name 'QStyleOptionMenuItem_checked';
procedure QStyleOptionMenuItem_setChecked(handle : QStyleOptionMenuItemH; checked : Boolean); cdecl; external QtIntf name 'QStyleOptionMenuItem_setChecked';
function QStyleOptionMenuItem_menuHasCheckableItems(handle : QStyleOptionMenuItemH) : Boolean; cdecl; external QtIntf name 'QStyleOptionMenuItem_menuHasCheckableItems';
procedure QStyleOptionMenuItem_setMenuHasCheckableItems(handle : QStyleOptionMenuItemH; menuHasCheckableItems : Boolean); cdecl; external QtIntf name 'QStyleOptionMenuItem_setMenuHasCheckableItems';
procedure QStyleOptionMenuItem_menuRect(handle : QStyleOptionMenuItemH; retval : PRect ); cdecl; external QtIntf name 'QStyleOptionMenuItem_menuRect';
procedure QStyleOptionMenuItem_setMenuRect(handle : QStyleOptionMenuItemH; menuRect :  PRect); cdecl; external QtIntf name 'QStyleOptionMenuItem_setMenuRect';
procedure QStyleOptionMenuItem_text(handle : QStyleOptionMenuItemH; retval : PWideString ); cdecl; external QtIntf name 'QStyleOptionMenuItem_text';
procedure QStyleOptionMenuItem_setText(handle : QStyleOptionMenuItemH; text :  PWideString); cdecl; external QtIntf name 'QStyleOptionMenuItem_setText';
procedure QStyleOptionMenuItem_icon(handle : QStyleOptionMenuItemH; retval : QIconH ); cdecl; external QtIntf name 'QStyleOptionMenuItem_icon';
procedure QStyleOptionMenuItem_setIcon(handle : QStyleOptionMenuItemH; icon :  QIconH); cdecl; external QtIntf name 'QStyleOptionMenuItem_setIcon';
function QStyleOptionMenuItem_maxIconWidth(handle : QStyleOptionMenuItemH) : Integer; cdecl; external QtIntf name 'QStyleOptionMenuItem_maxIconWidth';
procedure QStyleOptionMenuItem_setMaxIconWidth(handle : QStyleOptionMenuItemH; maxIconWidth : Integer); cdecl; external QtIntf name 'QStyleOptionMenuItem_setMaxIconWidth';
function QStyleOptionMenuItem_tabWidth(handle : QStyleOptionMenuItemH) : Integer; cdecl; external QtIntf name 'QStyleOptionMenuItem_tabWidth';
procedure QStyleOptionMenuItem_setTabWidth(handle : QStyleOptionMenuItemH; tabWidth : Integer); cdecl; external QtIntf name 'QStyleOptionMenuItem_setTabWidth';
procedure QStyleOptionMenuItem_font(handle : QStyleOptionMenuItemH; retval : QFontH ); cdecl; external QtIntf name 'QStyleOptionMenuItem_font';
procedure QStyleOptionMenuItem_setFont(handle : QStyleOptionMenuItemH; font :  QFontH); cdecl; external QtIntf name 'QStyleOptionMenuItem_setFont';
function QStyleOptionMenuItem_create(): QStyleOptionMenuItemH; overload; cdecl; external QtIntf name 'QStyleOptionMenuItem_create';
procedure QStyleOptionMenuItem_destroy(handle: QStyleOptionMenuItemH); cdecl; external QtIntf name 'QStyleOptionMenuItem_destroy'; 
function QStyleOptionMenuItem_create(other: QStyleOptionMenuItemH): QStyleOptionMenuItemH; overload; cdecl; external QtIntf name 'QStyleOptionMenuItem_create2';

function QStyleOptionQ3ListViewItem_features(handle : QStyleOptionQ3ListViewItemH) : QStyleOptionQ3ListViewItemQ3ListViewItemFeatures; cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_features';
procedure QStyleOptionQ3ListViewItem_setFeatures(handle : QStyleOptionQ3ListViewItemH; features : QStyleOptionQ3ListViewItemQ3ListViewItemFeatures); cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_setFeatures';
function QStyleOptionQ3ListViewItem_height(handle : QStyleOptionQ3ListViewItemH) : Integer; cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_height';
procedure QStyleOptionQ3ListViewItem_setHeight(handle : QStyleOptionQ3ListViewItemH; height : Integer); cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_setHeight';
function QStyleOptionQ3ListViewItem_totalHeight(handle : QStyleOptionQ3ListViewItemH) : Integer; cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_totalHeight';
procedure QStyleOptionQ3ListViewItem_setTotalHeight(handle : QStyleOptionQ3ListViewItemH; totalHeight : Integer); cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_setTotalHeight';
function QStyleOptionQ3ListViewItem_itemY(handle : QStyleOptionQ3ListViewItemH) : Integer; cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_itemY';
procedure QStyleOptionQ3ListViewItem_setItemY(handle : QStyleOptionQ3ListViewItemH; itemY : Integer); cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_setItemY';
function QStyleOptionQ3ListViewItem_childCount(handle : QStyleOptionQ3ListViewItemH) : Integer; cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_childCount';
procedure QStyleOptionQ3ListViewItem_setChildCount(handle : QStyleOptionQ3ListViewItemH; childCount : Integer); cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_setChildCount';
function QStyleOptionQ3ListViewItem_create(): QStyleOptionQ3ListViewItemH; overload; cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_create';
procedure QStyleOptionQ3ListViewItem_destroy(handle: QStyleOptionQ3ListViewItemH); cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_destroy'; 
function QStyleOptionQ3ListViewItem_create(other: QStyleOptionQ3ListViewItemH): QStyleOptionQ3ListViewItemH; overload; cdecl; external QtIntf name 'QStyleOptionQ3ListViewItem_create2';

function QStyleOptionQ3DockWindow_docked(handle : QStyleOptionQ3DockWindowH) : Boolean; cdecl; external QtIntf name 'QStyleOptionQ3DockWindow_docked';
procedure QStyleOptionQ3DockWindow_setDocked(handle : QStyleOptionQ3DockWindowH; docked : Boolean); cdecl; external QtIntf name 'QStyleOptionQ3DockWindow_setDocked';
function QStyleOptionQ3DockWindow_closeEnabled(handle : QStyleOptionQ3DockWindowH) : Boolean; cdecl; external QtIntf name 'QStyleOptionQ3DockWindow_closeEnabled';
procedure QStyleOptionQ3DockWindow_setCloseEnabled(handle : QStyleOptionQ3DockWindowH; closeEnabled : Boolean); cdecl; external QtIntf name 'QStyleOptionQ3DockWindow_setCloseEnabled';
function QStyleOptionQ3DockWindow_create(): QStyleOptionQ3DockWindowH; overload; cdecl; external QtIntf name 'QStyleOptionQ3DockWindow_create';
procedure QStyleOptionQ3DockWindow_destroy(handle: QStyleOptionQ3DockWindowH); cdecl; external QtIntf name 'QStyleOptionQ3DockWindow_destroy'; 
function QStyleOptionQ3DockWindow_create(other: QStyleOptionQ3DockWindowH): QStyleOptionQ3DockWindowH; overload; cdecl; external QtIntf name 'QStyleOptionQ3DockWindow_create2';

procedure QStyleOptionDockWidget_title(handle : QStyleOptionDockWidgetH; retval : PWideString ); cdecl; external QtIntf name 'QStyleOptionDockWidget_title';
procedure QStyleOptionDockWidget_setTitle(handle : QStyleOptionDockWidgetH; title :  PWideString); cdecl; external QtIntf name 'QStyleOptionDockWidget_setTitle';
function QStyleOptionDockWidget_closable(handle : QStyleOptionDockWidgetH) : Boolean; cdecl; external QtIntf name 'QStyleOptionDockWidget_closable';
procedure QStyleOptionDockWidget_setClosable(handle : QStyleOptionDockWidgetH; closable : Boolean); cdecl; external QtIntf name 'QStyleOptionDockWidget_setClosable';
function QStyleOptionDockWidget_movable(handle : QStyleOptionDockWidgetH) : Boolean; cdecl; external QtIntf name 'QStyleOptionDockWidget_movable';
procedure QStyleOptionDockWidget_setMovable(handle : QStyleOptionDockWidgetH; movable : Boolean); cdecl; external QtIntf name 'QStyleOptionDockWidget_setMovable';
function QStyleOptionDockWidget_floatable(handle : QStyleOptionDockWidgetH) : Boolean; cdecl; external QtIntf name 'QStyleOptionDockWidget_floatable';
procedure QStyleOptionDockWidget_setFloatable(handle : QStyleOptionDockWidgetH; floatable : Boolean); cdecl; external QtIntf name 'QStyleOptionDockWidget_setFloatable';
function QStyleOptionDockWidget_create(): QStyleOptionDockWidgetH; overload; cdecl; external QtIntf name 'QStyleOptionDockWidget_create';
procedure QStyleOptionDockWidget_destroy(handle: QStyleOptionDockWidgetH); cdecl; external QtIntf name 'QStyleOptionDockWidget_destroy'; 
function QStyleOptionDockWidget_create(other: QStyleOptionDockWidgetH): QStyleOptionDockWidgetH; overload; cdecl; external QtIntf name 'QStyleOptionDockWidget_create2';

function QStyleOptionViewItem_displayAlignment(handle : QStyleOptionViewItemH) : QtAlignment; cdecl; external QtIntf name 'QStyleOptionViewItem_displayAlignment';
procedure QStyleOptionViewItem_setDisplayAlignment(handle : QStyleOptionViewItemH; displayAlignment : QtAlignment); cdecl; external QtIntf name 'QStyleOptionViewItem_setDisplayAlignment';
function QStyleOptionViewItem_decorationAlignment(handle : QStyleOptionViewItemH) : QtAlignment; cdecl; external QtIntf name 'QStyleOptionViewItem_decorationAlignment';
procedure QStyleOptionViewItem_setDecorationAlignment(handle : QStyleOptionViewItemH; decorationAlignment : QtAlignment); cdecl; external QtIntf name 'QStyleOptionViewItem_setDecorationAlignment';
function QStyleOptionViewItem_textElideMode(handle : QStyleOptionViewItemH) : QtTextElideMode; cdecl; external QtIntf name 'QStyleOptionViewItem_textElideMode';
procedure QStyleOptionViewItem_setTextElideMode(handle : QStyleOptionViewItemH; textElideMode : QtTextElideMode); cdecl; external QtIntf name 'QStyleOptionViewItem_setTextElideMode';
function QStyleOptionViewItem_decorationPosition(handle : QStyleOptionViewItemH) : QStyleOptionViewItemPosition; cdecl; external QtIntf name 'QStyleOptionViewItem_decorationPosition';
procedure QStyleOptionViewItem_setDecorationPosition(handle : QStyleOptionViewItemH; decorationPosition : QStyleOptionViewItemPosition); cdecl; external QtIntf name 'QStyleOptionViewItem_setDecorationPosition';
procedure QStyleOptionViewItem_decorationSize(handle : QStyleOptionViewItemH; retval : PSize ); cdecl; external QtIntf name 'QStyleOptionViewItem_decorationSize';
procedure QStyleOptionViewItem_setDecorationSize(handle : QStyleOptionViewItemH; decorationSize :  PSize); cdecl; external QtIntf name 'QStyleOptionViewItem_setDecorationSize';
procedure QStyleOptionViewItem_font(handle : QStyleOptionViewItemH; retval : QFontH ); cdecl; external QtIntf name 'QStyleOptionViewItem_font';
procedure QStyleOptionViewItem_setFont(handle : QStyleOptionViewItemH; font :  QFontH); cdecl; external QtIntf name 'QStyleOptionViewItem_setFont';
function QStyleOptionViewItem_showDecorationSelected(handle : QStyleOptionViewItemH) : Boolean; cdecl; external QtIntf name 'QStyleOptionViewItem_showDecorationSelected';
procedure QStyleOptionViewItem_setShowDecorationSelected(handle : QStyleOptionViewItemH; showDecorationSelected : Boolean); cdecl; external QtIntf name 'QStyleOptionViewItem_setShowDecorationSelected';
function QStyleOptionViewItem_create(): QStyleOptionViewItemH; overload; cdecl; external QtIntf name 'QStyleOptionViewItem_create';
procedure QStyleOptionViewItem_destroy(handle: QStyleOptionViewItemH); cdecl; external QtIntf name 'QStyleOptionViewItem_destroy'; 
function QStyleOptionViewItem_create(other: QStyleOptionViewItemH): QStyleOptionViewItemH; overload; cdecl; external QtIntf name 'QStyleOptionViewItem_create2';

function QStyleOptionViewItemV2_features(handle : QStyleOptionViewItemV2H) : QStyleOptionViewItemV2ViewItemFeatures; cdecl; external QtIntf name 'QStyleOptionViewItemV2_features';
procedure QStyleOptionViewItemV2_setFeatures(handle : QStyleOptionViewItemV2H; features : QStyleOptionViewItemV2ViewItemFeatures); cdecl; external QtIntf name 'QStyleOptionViewItemV2_setFeatures';
function QStyleOptionViewItemV2_create(): QStyleOptionViewItemV2H; overload; cdecl; external QtIntf name 'QStyleOptionViewItemV2_create';
procedure QStyleOptionViewItemV2_destroy(handle: QStyleOptionViewItemV2H); cdecl; external QtIntf name 'QStyleOptionViewItemV2_destroy'; 
function QStyleOptionViewItemV2_create(other: QStyleOptionViewItemV2H): QStyleOptionViewItemV2H; overload; cdecl; external QtIntf name 'QStyleOptionViewItemV2_create2';
function QStyleOptionViewItemV2_create(other: QStyleOptionViewItemH): QStyleOptionViewItemV2H; overload; cdecl; external QtIntf name 'QStyleOptionViewItemV2_create3';

procedure QStyleOptionToolBox_text(handle : QStyleOptionToolBoxH; retval : PWideString ); cdecl; external QtIntf name 'QStyleOptionToolBox_text';
procedure QStyleOptionToolBox_setText(handle : QStyleOptionToolBoxH; text :  PWideString); cdecl; external QtIntf name 'QStyleOptionToolBox_setText';
procedure QStyleOptionToolBox_icon(handle : QStyleOptionToolBoxH; retval : QIconH ); cdecl; external QtIntf name 'QStyleOptionToolBox_icon';
procedure QStyleOptionToolBox_setIcon(handle : QStyleOptionToolBoxH; icon :  QIconH); cdecl; external QtIntf name 'QStyleOptionToolBox_setIcon';
function QStyleOptionToolBox_create(): QStyleOptionToolBoxH; overload; cdecl; external QtIntf name 'QStyleOptionToolBox_create';
procedure QStyleOptionToolBox_destroy(handle: QStyleOptionToolBoxH); cdecl; external QtIntf name 'QStyleOptionToolBox_destroy'; 
function QStyleOptionToolBox_create(other: QStyleOptionToolBoxH): QStyleOptionToolBoxH; overload; cdecl; external QtIntf name 'QStyleOptionToolBox_create2';

function QStyleOptionRubberBand_shape(handle : QStyleOptionRubberBandH) : QRubberBandShape; cdecl; external QtIntf name 'QStyleOptionRubberBand_shape';
procedure QStyleOptionRubberBand_setShape(handle : QStyleOptionRubberBandH; shape : QRubberBandShape); cdecl; external QtIntf name 'QStyleOptionRubberBand_setShape';
function QStyleOptionRubberBand_opaque(handle : QStyleOptionRubberBandH) : Boolean; cdecl; external QtIntf name 'QStyleOptionRubberBand_opaque';
procedure QStyleOptionRubberBand_setOpaque(handle : QStyleOptionRubberBandH; opaque : Boolean); cdecl; external QtIntf name 'QStyleOptionRubberBand_setOpaque';
function QStyleOptionRubberBand_create(): QStyleOptionRubberBandH; overload; cdecl; external QtIntf name 'QStyleOptionRubberBand_create';
procedure QStyleOptionRubberBand_destroy(handle: QStyleOptionRubberBandH); cdecl; external QtIntf name 'QStyleOptionRubberBand_destroy'; 
function QStyleOptionRubberBand_create(other: QStyleOptionRubberBandH): QStyleOptionRubberBandH; overload; cdecl; external QtIntf name 'QStyleOptionRubberBand_create2';

function QStyleOptionComplex_subControls(handle : QStyleOptionComplexH) : QStyleSubControls; cdecl; external QtIntf name 'QStyleOptionComplex_subControls';
procedure QStyleOptionComplex_setSubControls(handle : QStyleOptionComplexH; subControls : QStyleSubControls); cdecl; external QtIntf name 'QStyleOptionComplex_setSubControls';
function QStyleOptionComplex_activeSubControls(handle : QStyleOptionComplexH) : QStyleSubControls; cdecl; external QtIntf name 'QStyleOptionComplex_activeSubControls';
procedure QStyleOptionComplex_setActiveSubControls(handle : QStyleOptionComplexH; activeSubControls : QStyleSubControls); cdecl; external QtIntf name 'QStyleOptionComplex_setActiveSubControls';
function QStyleOptionComplex_create(version: Integer; _type: Integer): QStyleOptionComplexH; overload; cdecl; external QtIntf name 'QStyleOptionComplex_create';
procedure QStyleOptionComplex_destroy(handle: QStyleOptionComplexH); cdecl; external QtIntf name 'QStyleOptionComplex_destroy'; 
function QStyleOptionComplex_create(other: QStyleOptionComplexH): QStyleOptionComplexH; overload; cdecl; external QtIntf name 'QStyleOptionComplex_create2';

function QStyleOptionSlider_orientation(handle : QStyleOptionSliderH) : QtOrientation; cdecl; external QtIntf name 'QStyleOptionSlider_orientation';
procedure QStyleOptionSlider_setOrientation(handle : QStyleOptionSliderH; orientation : QtOrientation); cdecl; external QtIntf name 'QStyleOptionSlider_setOrientation';
function QStyleOptionSlider_minimum(handle : QStyleOptionSliderH) : Integer; cdecl; external QtIntf name 'QStyleOptionSlider_minimum';
procedure QStyleOptionSlider_setMinimum(handle : QStyleOptionSliderH; minimum : Integer); cdecl; external QtIntf name 'QStyleOptionSlider_setMinimum';
function QStyleOptionSlider_maximum(handle : QStyleOptionSliderH) : Integer; cdecl; external QtIntf name 'QStyleOptionSlider_maximum';
procedure QStyleOptionSlider_setMaximum(handle : QStyleOptionSliderH; maximum : Integer); cdecl; external QtIntf name 'QStyleOptionSlider_setMaximum';
function QStyleOptionSlider_tickPosition(handle : QStyleOptionSliderH) : QSliderTickPosition; cdecl; external QtIntf name 'QStyleOptionSlider_tickPosition';
procedure QStyleOptionSlider_setTickPosition(handle : QStyleOptionSliderH; tickPosition : QSliderTickPosition); cdecl; external QtIntf name 'QStyleOptionSlider_setTickPosition';
function QStyleOptionSlider_tickInterval(handle : QStyleOptionSliderH) : Integer; cdecl; external QtIntf name 'QStyleOptionSlider_tickInterval';
procedure QStyleOptionSlider_setTickInterval(handle : QStyleOptionSliderH; tickInterval : Integer); cdecl; external QtIntf name 'QStyleOptionSlider_setTickInterval';
function QStyleOptionSlider_upsideDown(handle : QStyleOptionSliderH) : Boolean; cdecl; external QtIntf name 'QStyleOptionSlider_upsideDown';
procedure QStyleOptionSlider_setUpsideDown(handle : QStyleOptionSliderH; upsideDown : Boolean); cdecl; external QtIntf name 'QStyleOptionSlider_setUpsideDown';
function QStyleOptionSlider_sliderPosition(handle : QStyleOptionSliderH) : Integer; cdecl; external QtIntf name 'QStyleOptionSlider_sliderPosition';
procedure QStyleOptionSlider_setSliderPosition(handle : QStyleOptionSliderH; sliderPosition : Integer); cdecl; external QtIntf name 'QStyleOptionSlider_setSliderPosition';
function QStyleOptionSlider_sliderValue(handle : QStyleOptionSliderH) : Integer; cdecl; external QtIntf name 'QStyleOptionSlider_sliderValue';
procedure QStyleOptionSlider_setSliderValue(handle : QStyleOptionSliderH; sliderValue : Integer); cdecl; external QtIntf name 'QStyleOptionSlider_setSliderValue';
function QStyleOptionSlider_singleStep(handle : QStyleOptionSliderH) : Integer; cdecl; external QtIntf name 'QStyleOptionSlider_singleStep';
procedure QStyleOptionSlider_setSingleStep(handle : QStyleOptionSliderH; singleStep : Integer); cdecl; external QtIntf name 'QStyleOptionSlider_setSingleStep';
function QStyleOptionSlider_pageStep(handle : QStyleOptionSliderH) : Integer; cdecl; external QtIntf name 'QStyleOptionSlider_pageStep';
procedure QStyleOptionSlider_setPageStep(handle : QStyleOptionSliderH; pageStep : Integer); cdecl; external QtIntf name 'QStyleOptionSlider_setPageStep';
function QStyleOptionSlider_notchTarget(handle : QStyleOptionSliderH) : Double; cdecl; external QtIntf name 'QStyleOptionSlider_notchTarget';
procedure QStyleOptionSlider_setNotchTarget(handle : QStyleOptionSliderH; notchTarget : Double); cdecl; external QtIntf name 'QStyleOptionSlider_setNotchTarget';
function QStyleOptionSlider_dialWrapping(handle : QStyleOptionSliderH) : Boolean; cdecl; external QtIntf name 'QStyleOptionSlider_dialWrapping';
procedure QStyleOptionSlider_setDialWrapping(handle : QStyleOptionSliderH; dialWrapping : Boolean); cdecl; external QtIntf name 'QStyleOptionSlider_setDialWrapping';
function QStyleOptionSlider_create(): QStyleOptionSliderH; overload; cdecl; external QtIntf name 'QStyleOptionSlider_create';
procedure QStyleOptionSlider_destroy(handle: QStyleOptionSliderH); cdecl; external QtIntf name 'QStyleOptionSlider_destroy'; 
function QStyleOptionSlider_create(other: QStyleOptionSliderH): QStyleOptionSliderH; overload; cdecl; external QtIntf name 'QStyleOptionSlider_create2';

function QStyleOptionSpinBox_buttonSymbols(handle : QStyleOptionSpinBoxH) : QAbstractSpinBoxButtonSymbols; cdecl; external QtIntf name 'QStyleOptionSpinBox_buttonSymbols';
procedure QStyleOptionSpinBox_setButtonSymbols(handle : QStyleOptionSpinBoxH; buttonSymbols : QAbstractSpinBoxButtonSymbols); cdecl; external QtIntf name 'QStyleOptionSpinBox_setButtonSymbols';
function QStyleOptionSpinBox_stepEnabled(handle : QStyleOptionSpinBoxH) : QAbstractSpinBoxStepEnabled; cdecl; external QtIntf name 'QStyleOptionSpinBox_stepEnabled';
procedure QStyleOptionSpinBox_setStepEnabled(handle : QStyleOptionSpinBoxH; stepEnabled : QAbstractSpinBoxStepEnabled); cdecl; external QtIntf name 'QStyleOptionSpinBox_setStepEnabled';
function QStyleOptionSpinBox_frame(handle : QStyleOptionSpinBoxH) : Boolean; cdecl; external QtIntf name 'QStyleOptionSpinBox_frame';
procedure QStyleOptionSpinBox_setFrame(handle : QStyleOptionSpinBoxH; frame : Boolean); cdecl; external QtIntf name 'QStyleOptionSpinBox_setFrame';
function QStyleOptionSpinBox_create(): QStyleOptionSpinBoxH; overload; cdecl; external QtIntf name 'QStyleOptionSpinBox_create';
procedure QStyleOptionSpinBox_destroy(handle: QStyleOptionSpinBoxH); cdecl; external QtIntf name 'QStyleOptionSpinBox_destroy'; 
function QStyleOptionSpinBox_create(other: QStyleOptionSpinBoxH): QStyleOptionSpinBoxH; overload; cdecl; external QtIntf name 'QStyleOptionSpinBox_create2';

procedure QStyleOptionQ3ListView_viewportPalette(handle : QStyleOptionQ3ListViewH; retval : QPaletteH ); cdecl; external QtIntf name 'QStyleOptionQ3ListView_viewportPalette';
procedure QStyleOptionQ3ListView_setViewportPalette(handle : QStyleOptionQ3ListViewH; viewportPalette :  QPaletteH); cdecl; external QtIntf name 'QStyleOptionQ3ListView_setViewportPalette';
function QStyleOptionQ3ListView_viewportBGRole(handle : QStyleOptionQ3ListViewH) : QPaletteColorRole; cdecl; external QtIntf name 'QStyleOptionQ3ListView_viewportBGRole';
procedure QStyleOptionQ3ListView_setViewportBGRole(handle : QStyleOptionQ3ListViewH; viewportBGRole : QPaletteColorRole); cdecl; external QtIntf name 'QStyleOptionQ3ListView_setViewportBGRole';
function QStyleOptionQ3ListView_sortColumn(handle : QStyleOptionQ3ListViewH) : Integer; cdecl; external QtIntf name 'QStyleOptionQ3ListView_sortColumn';
procedure QStyleOptionQ3ListView_setSortColumn(handle : QStyleOptionQ3ListViewH; sortColumn : Integer); cdecl; external QtIntf name 'QStyleOptionQ3ListView_setSortColumn';
function QStyleOptionQ3ListView_itemMargin(handle : QStyleOptionQ3ListViewH) : Integer; cdecl; external QtIntf name 'QStyleOptionQ3ListView_itemMargin';
procedure QStyleOptionQ3ListView_setItemMargin(handle : QStyleOptionQ3ListViewH; itemMargin : Integer); cdecl; external QtIntf name 'QStyleOptionQ3ListView_setItemMargin';
function QStyleOptionQ3ListView_treeStepSize(handle : QStyleOptionQ3ListViewH) : Integer; cdecl; external QtIntf name 'QStyleOptionQ3ListView_treeStepSize';
procedure QStyleOptionQ3ListView_setTreeStepSize(handle : QStyleOptionQ3ListViewH; treeStepSize : Integer); cdecl; external QtIntf name 'QStyleOptionQ3ListView_setTreeStepSize';
function QStyleOptionQ3ListView_rootIsDecorated(handle : QStyleOptionQ3ListViewH) : Boolean; cdecl; external QtIntf name 'QStyleOptionQ3ListView_rootIsDecorated';
procedure QStyleOptionQ3ListView_setRootIsDecorated(handle : QStyleOptionQ3ListViewH; rootIsDecorated : Boolean); cdecl; external QtIntf name 'QStyleOptionQ3ListView_setRootIsDecorated';
function QStyleOptionQ3ListView_create(): QStyleOptionQ3ListViewH; overload; cdecl; external QtIntf name 'QStyleOptionQ3ListView_create';
procedure QStyleOptionQ3ListView_destroy(handle: QStyleOptionQ3ListViewH); cdecl; external QtIntf name 'QStyleOptionQ3ListView_destroy'; 
function QStyleOptionQ3ListView_create(other: QStyleOptionQ3ListViewH): QStyleOptionQ3ListViewH; overload; cdecl; external QtIntf name 'QStyleOptionQ3ListView_create2';

function QStyleOptionToolButton_features(handle : QStyleOptionToolButtonH) : QStyleOptionToolButtonToolButtonFeatures; cdecl; external QtIntf name 'QStyleOptionToolButton_features';
procedure QStyleOptionToolButton_setFeatures(handle : QStyleOptionToolButtonH; features : QStyleOptionToolButtonToolButtonFeatures); cdecl; external QtIntf name 'QStyleOptionToolButton_setFeatures';
procedure QStyleOptionToolButton_icon(handle : QStyleOptionToolButtonH; retval : QIconH ); cdecl; external QtIntf name 'QStyleOptionToolButton_icon';
procedure QStyleOptionToolButton_setIcon(handle : QStyleOptionToolButtonH; icon :  QIconH); cdecl; external QtIntf name 'QStyleOptionToolButton_setIcon';
procedure QStyleOptionToolButton_iconSize(handle : QStyleOptionToolButtonH; retval : PSize ); cdecl; external QtIntf name 'QStyleOptionToolButton_iconSize';
procedure QStyleOptionToolButton_setIconSize(handle : QStyleOptionToolButtonH; iconSize :  PSize); cdecl; external QtIntf name 'QStyleOptionToolButton_setIconSize';
procedure QStyleOptionToolButton_text(handle : QStyleOptionToolButtonH; retval : PWideString ); cdecl; external QtIntf name 'QStyleOptionToolButton_text';
procedure QStyleOptionToolButton_setText(handle : QStyleOptionToolButtonH; text :  PWideString); cdecl; external QtIntf name 'QStyleOptionToolButton_setText';
function QStyleOptionToolButton_arrowType(handle : QStyleOptionToolButtonH) : QtArrowType; cdecl; external QtIntf name 'QStyleOptionToolButton_arrowType';
procedure QStyleOptionToolButton_setArrowType(handle : QStyleOptionToolButtonH; arrowType : QtArrowType); cdecl; external QtIntf name 'QStyleOptionToolButton_setArrowType';
function QStyleOptionToolButton_toolButtonStyle(handle : QStyleOptionToolButtonH) : QtToolButtonStyle; cdecl; external QtIntf name 'QStyleOptionToolButton_toolButtonStyle';
procedure QStyleOptionToolButton_setToolButtonStyle(handle : QStyleOptionToolButtonH; toolButtonStyle : QtToolButtonStyle); cdecl; external QtIntf name 'QStyleOptionToolButton_setToolButtonStyle';
procedure QStyleOptionToolButton_pos(handle : QStyleOptionToolButtonH; retval : PQtPoint ); cdecl; external QtIntf name 'QStyleOptionToolButton_pos';
procedure QStyleOptionToolButton_setPos(handle : QStyleOptionToolButtonH; pos :  PQtPoint); cdecl; external QtIntf name 'QStyleOptionToolButton_setPos';
procedure QStyleOptionToolButton_font(handle : QStyleOptionToolButtonH; retval : QFontH ); cdecl; external QtIntf name 'QStyleOptionToolButton_font';
procedure QStyleOptionToolButton_setFont(handle : QStyleOptionToolButtonH; font :  QFontH); cdecl; external QtIntf name 'QStyleOptionToolButton_setFont';
function QStyleOptionToolButton_create(): QStyleOptionToolButtonH; overload; cdecl; external QtIntf name 'QStyleOptionToolButton_create';
procedure QStyleOptionToolButton_destroy(handle: QStyleOptionToolButtonH); cdecl; external QtIntf name 'QStyleOptionToolButton_destroy'; 
function QStyleOptionToolButton_create(other: QStyleOptionToolButtonH): QStyleOptionToolButtonH; overload; cdecl; external QtIntf name 'QStyleOptionToolButton_create2';

function QStyleOptionComboBox_editable(handle : QStyleOptionComboBoxH) : Boolean; cdecl; external QtIntf name 'QStyleOptionComboBox_editable';
procedure QStyleOptionComboBox_setEditable(handle : QStyleOptionComboBoxH; editable : Boolean); cdecl; external QtIntf name 'QStyleOptionComboBox_setEditable';
procedure QStyleOptionComboBox_popupRect(handle : QStyleOptionComboBoxH; retval : PRect ); cdecl; external QtIntf name 'QStyleOptionComboBox_popupRect';
procedure QStyleOptionComboBox_setPopupRect(handle : QStyleOptionComboBoxH; popupRect :  PRect); cdecl; external QtIntf name 'QStyleOptionComboBox_setPopupRect';
function QStyleOptionComboBox_frame(handle : QStyleOptionComboBoxH) : Boolean; cdecl; external QtIntf name 'QStyleOptionComboBox_frame';
procedure QStyleOptionComboBox_setFrame(handle : QStyleOptionComboBoxH; frame : Boolean); cdecl; external QtIntf name 'QStyleOptionComboBox_setFrame';
procedure QStyleOptionComboBox_currentText(handle : QStyleOptionComboBoxH; retval : PWideString ); cdecl; external QtIntf name 'QStyleOptionComboBox_currentText';
procedure QStyleOptionComboBox_setCurrentText(handle : QStyleOptionComboBoxH; currentText :  PWideString); cdecl; external QtIntf name 'QStyleOptionComboBox_setCurrentText';
procedure QStyleOptionComboBox_currentIcon(handle : QStyleOptionComboBoxH; retval : QIconH ); cdecl; external QtIntf name 'QStyleOptionComboBox_currentIcon';
procedure QStyleOptionComboBox_setCurrentIcon(handle : QStyleOptionComboBoxH; currentIcon :  QIconH); cdecl; external QtIntf name 'QStyleOptionComboBox_setCurrentIcon';
procedure QStyleOptionComboBox_iconSize(handle : QStyleOptionComboBoxH; retval : PSize ); cdecl; external QtIntf name 'QStyleOptionComboBox_iconSize';
procedure QStyleOptionComboBox_setIconSize(handle : QStyleOptionComboBoxH; iconSize :  PSize); cdecl; external QtIntf name 'QStyleOptionComboBox_setIconSize';
function QStyleOptionComboBox_create(): QStyleOptionComboBoxH; overload; cdecl; external QtIntf name 'QStyleOptionComboBox_create';
procedure QStyleOptionComboBox_destroy(handle: QStyleOptionComboBoxH); cdecl; external QtIntf name 'QStyleOptionComboBox_destroy'; 
function QStyleOptionComboBox_create(other: QStyleOptionComboBoxH): QStyleOptionComboBoxH; overload; cdecl; external QtIntf name 'QStyleOptionComboBox_create2';

procedure QStyleOptionTitleBar_text(handle : QStyleOptionTitleBarH; retval : PWideString ); cdecl; external QtIntf name 'QStyleOptionTitleBar_text';
procedure QStyleOptionTitleBar_setText(handle : QStyleOptionTitleBarH; text :  PWideString); cdecl; external QtIntf name 'QStyleOptionTitleBar_setText';
procedure QStyleOptionTitleBar_icon(handle : QStyleOptionTitleBarH; retval : QIconH ); cdecl; external QtIntf name 'QStyleOptionTitleBar_icon';
procedure QStyleOptionTitleBar_setIcon(handle : QStyleOptionTitleBarH; icon :  QIconH); cdecl; external QtIntf name 'QStyleOptionTitleBar_setIcon';
function QStyleOptionTitleBar_titleBarState(handle : QStyleOptionTitleBarH) : Integer; cdecl; external QtIntf name 'QStyleOptionTitleBar_titleBarState';
procedure QStyleOptionTitleBar_setTitleBarState(handle : QStyleOptionTitleBarH; titleBarState : Integer); cdecl; external QtIntf name 'QStyleOptionTitleBar_setTitleBarState';
function QStyleOptionTitleBar_titleBarFlags(handle : QStyleOptionTitleBarH) : QtWindowFlags; cdecl; external QtIntf name 'QStyleOptionTitleBar_titleBarFlags';
procedure QStyleOptionTitleBar_setTitleBarFlags(handle : QStyleOptionTitleBarH; titleBarFlags : QtWindowFlags); cdecl; external QtIntf name 'QStyleOptionTitleBar_setTitleBarFlags';
function QStyleOptionTitleBar_create(): QStyleOptionTitleBarH; overload; cdecl; external QtIntf name 'QStyleOptionTitleBar_create';
procedure QStyleOptionTitleBar_destroy(handle: QStyleOptionTitleBarH); cdecl; external QtIntf name 'QStyleOptionTitleBar_destroy'; 
function QStyleOptionTitleBar_create(other: QStyleOptionTitleBarH): QStyleOptionTitleBarH; overload; cdecl; external QtIntf name 'QStyleOptionTitleBar_create2';

function QStyleOptionGroupBox_features(handle : QStyleOptionGroupBoxH) : QStyleOptionFrameV2FrameFeatures; cdecl; external QtIntf name 'QStyleOptionGroupBox_features';
procedure QStyleOptionGroupBox_setFeatures(handle : QStyleOptionGroupBoxH; features : QStyleOptionFrameV2FrameFeatures); cdecl; external QtIntf name 'QStyleOptionGroupBox_setFeatures';
procedure QStyleOptionGroupBox_text(handle : QStyleOptionGroupBoxH; retval : PWideString ); cdecl; external QtIntf name 'QStyleOptionGroupBox_text';
procedure QStyleOptionGroupBox_setText(handle : QStyleOptionGroupBoxH; text :  PWideString); cdecl; external QtIntf name 'QStyleOptionGroupBox_setText';
function QStyleOptionGroupBox_textAlignment(handle : QStyleOptionGroupBoxH) : QtAlignment; cdecl; external QtIntf name 'QStyleOptionGroupBox_textAlignment';
procedure QStyleOptionGroupBox_setTextAlignment(handle : QStyleOptionGroupBoxH; textAlignment : QtAlignment); cdecl; external QtIntf name 'QStyleOptionGroupBox_setTextAlignment';
procedure QStyleOptionGroupBox_textColor(handle : QStyleOptionGroupBoxH; retval : PQColor ); cdecl; external QtIntf name 'QStyleOptionGroupBox_textColor';
procedure QStyleOptionGroupBox_setTextColor(handle : QStyleOptionGroupBoxH; textColor :  PQColor); cdecl; external QtIntf name 'QStyleOptionGroupBox_setTextColor';
function QStyleOptionGroupBox_lineWidth(handle : QStyleOptionGroupBoxH) : Integer; cdecl; external QtIntf name 'QStyleOptionGroupBox_lineWidth';
procedure QStyleOptionGroupBox_setLineWidth(handle : QStyleOptionGroupBoxH; lineWidth : Integer); cdecl; external QtIntf name 'QStyleOptionGroupBox_setLineWidth';
function QStyleOptionGroupBox_midLineWidth(handle : QStyleOptionGroupBoxH) : Integer; cdecl; external QtIntf name 'QStyleOptionGroupBox_midLineWidth';
procedure QStyleOptionGroupBox_setMidLineWidth(handle : QStyleOptionGroupBoxH; midLineWidth : Integer); cdecl; external QtIntf name 'QStyleOptionGroupBox_setMidLineWidth';
function QStyleOptionGroupBox_create(): QStyleOptionGroupBoxH; overload; cdecl; external QtIntf name 'QStyleOptionGroupBox_create';
procedure QStyleOptionGroupBox_destroy(handle: QStyleOptionGroupBoxH); cdecl; external QtIntf name 'QStyleOptionGroupBox_destroy'; 
function QStyleOptionGroupBox_create(other: QStyleOptionGroupBoxH): QStyleOptionGroupBoxH; overload; cdecl; external QtIntf name 'QStyleOptionGroupBox_create2';

function QStyleOptionSizeGrip_corner(handle : QStyleOptionSizeGripH) : QtCorner; cdecl; external QtIntf name 'QStyleOptionSizeGrip_corner';
procedure QStyleOptionSizeGrip_setCorner(handle : QStyleOptionSizeGripH; corner : QtCorner); cdecl; external QtIntf name 'QStyleOptionSizeGrip_setCorner';
function QStyleOptionSizeGrip_create(): QStyleOptionSizeGripH; overload; cdecl; external QtIntf name 'QStyleOptionSizeGrip_create';
procedure QStyleOptionSizeGrip_destroy(handle: QStyleOptionSizeGripH); cdecl; external QtIntf name 'QStyleOptionSizeGrip_destroy'; 
function QStyleOptionSizeGrip_create(other: QStyleOptionSizeGripH): QStyleOptionSizeGripH; overload; cdecl; external QtIntf name 'QStyleOptionSizeGrip_create2';

procedure QStyleOptionGraphicsItem_exposedRect(handle : QStyleOptionGraphicsItemH; retval : QRectFH ); cdecl; external QtIntf name 'QStyleOptionGraphicsItem_exposedRect';
procedure QStyleOptionGraphicsItem_setExposedRect(handle : QStyleOptionGraphicsItemH; exposedRect :  QRectFH); cdecl; external QtIntf name 'QStyleOptionGraphicsItem_setExposedRect';
procedure QStyleOptionGraphicsItem_matrix(handle : QStyleOptionGraphicsItemH; retval : QMatrixH ); cdecl; external QtIntf name 'QStyleOptionGraphicsItem_matrix';
procedure QStyleOptionGraphicsItem_setMatrix(handle : QStyleOptionGraphicsItemH; matrix :  QMatrixH); cdecl; external QtIntf name 'QStyleOptionGraphicsItem_setMatrix';
function QStyleOptionGraphicsItem_levelOfDetail(handle : QStyleOptionGraphicsItemH) : Double; cdecl; external QtIntf name 'QStyleOptionGraphicsItem_levelOfDetail';
procedure QStyleOptionGraphicsItem_setLevelOfDetail(handle : QStyleOptionGraphicsItemH; levelOfDetail : Double); cdecl; external QtIntf name 'QStyleOptionGraphicsItem_setLevelOfDetail';
function QStyleOptionGraphicsItem_create(): QStyleOptionGraphicsItemH; overload; cdecl; external QtIntf name 'QStyleOptionGraphicsItem_create';
procedure QStyleOptionGraphicsItem_destroy(handle: QStyleOptionGraphicsItemH); cdecl; external QtIntf name 'QStyleOptionGraphicsItem_destroy'; 
function QStyleOptionGraphicsItem_create(other: QStyleOptionGraphicsItemH): QStyleOptionGraphicsItemH; overload; cdecl; external QtIntf name 'QStyleOptionGraphicsItem_create2';

function QStyleHintReturn_version(handle : QStyleHintReturnH) : Integer; cdecl; external QtIntf name 'QStyleHintReturn_version';
procedure QStyleHintReturn_setVersion(handle : QStyleHintReturnH; version : Integer); cdecl; external QtIntf name 'QStyleHintReturn_setVersion';
function QStyleHintReturn__type(handle : QStyleHintReturnH) : Integer; cdecl; external QtIntf name 'QStyleHintReturn__type';
procedure QStyleHintReturn_setType(handle : QStyleHintReturnH; _type : Integer); cdecl; external QtIntf name 'QStyleHintReturn_setType';
function QStyleHintReturn_create(version: Integer = QStyleOptionVersion; _type: Integer = QStyleHintReturnSH_Default): QStyleHintReturnH; cdecl; external QtIntf name 'QStyleHintReturn_create';
procedure QStyleHintReturn_destroy(handle: QStyleHintReturnH); cdecl; external QtIntf name 'QStyleHintReturn_destroy'; 

procedure QStyleHintReturnMask_region(handle : QStyleHintReturnMaskH; retval : QRegionH ); cdecl; external QtIntf name 'QStyleHintReturnMask_region';
procedure QStyleHintReturnMask_setRegion(handle : QStyleHintReturnMaskH; region :  QRegionH); cdecl; external QtIntf name 'QStyleHintReturnMask_setRegion';
function QStyleHintReturnMask_create(): QStyleHintReturnMaskH; cdecl; external QtIntf name 'QStyleHintReturnMask_create';
procedure QStyleHintReturnMask_destroy(handle: QStyleHintReturnMaskH); cdecl; external QtIntf name 'QStyleHintReturnMask_destroy'; 

procedure QStyleFactory_keys(retval: QStringListH); cdecl; external QtIntf name 'QStyleFactory_keys';
function QStyleFactory_create(p1: PWideString): QStyleH; cdecl; external QtIntf name 'QStyleFactory_create';


type
  QGraphicsSceneItemIndexMethod = (  //QGraphicsScene::ItemIndexMethod (2)
    QGraphicsSceneBspTreeIndex,
    QGraphicsSceneNoIndex = -1 );

function QGraphicsScene_create(parent: QObjectH = nil): QGraphicsSceneH; overload; cdecl; external QtIntf name 'QGraphicsScene_create';
procedure QGraphicsScene_destroy(handle: QGraphicsSceneH); cdecl; external QtIntf name 'QGraphicsScene_destroy'; 
function QGraphicsScene_create(sceneRect: QRectFH; parent: QObjectH = nil): QGraphicsSceneH; overload; cdecl; external QtIntf name 'QGraphicsScene_create2';
function QGraphicsScene_create(x: Double; y: Double; width: Double; height: Double; parent: QObjectH = nil): QGraphicsSceneH; overload; cdecl; external QtIntf name 'QGraphicsScene_create3';
procedure QGraphicsScene_sceneRect(handle: QGraphicsSceneH; retval: QRectFH); cdecl; external QtIntf name 'QGraphicsScene_sceneRect';
function QGraphicsScene_width(handle: QGraphicsSceneH): Double; cdecl; external QtIntf name 'QGraphicsScene_width';
function QGraphicsScene_height(handle: QGraphicsSceneH): Double; cdecl; external QtIntf name 'QGraphicsScene_height';
procedure QGraphicsScene_setSceneRect(handle: QGraphicsSceneH; rect: QRectFH); overload; cdecl; external QtIntf name 'QGraphicsScene_setSceneRect';
procedure QGraphicsScene_setSceneRect(handle: QGraphicsSceneH; x: Double; y: Double; w: Double; h: Double); overload; cdecl; external QtIntf name 'QGraphicsScene_setSceneRect2';
procedure QGraphicsScene_render(handle: QGraphicsSceneH; painter: QPainterH; target: QRectFH = nil; source: QRectFH = nil; aspectRatioMode: QtAspectRatioMode = QtKeepAspectRatio); cdecl; external QtIntf name 'QGraphicsScene_render';
function QGraphicsScene_itemIndexMethod(handle: QGraphicsSceneH): QGraphicsSceneItemIndexMethod; cdecl; external QtIntf name 'QGraphicsScene_itemIndexMethod';
procedure QGraphicsScene_setItemIndexMethod(handle: QGraphicsSceneH; method: QGraphicsSceneItemIndexMethod); cdecl; external QtIntf name 'QGraphicsScene_setItemIndexMethod';
procedure QGraphicsScene_itemsBoundingRect(handle: QGraphicsSceneH; retval: QRectFH); cdecl; external QtIntf name 'QGraphicsScene_itemsBoundingRect';
function QGraphicsScene_itemAt(handle: QGraphicsSceneH; pos: QPointFH): QGraphicsItemH; overload; cdecl; external QtIntf name 'QGraphicsScene_itemAt';
function QGraphicsScene_itemAt(handle: QGraphicsSceneH; x: Double; y: Double): QGraphicsItemH; overload; cdecl; external QtIntf name 'QGraphicsScene_itemAt2';
procedure QGraphicsScene_setSelectionArea(handle: QGraphicsSceneH; path: QPainterPathH); cdecl; external QtIntf name 'QGraphicsScene_setSelectionArea';
procedure QGraphicsScene_clearSelection(handle: QGraphicsSceneH); cdecl; external QtIntf name 'QGraphicsScene_clearSelection';
procedure QGraphicsScene_destroyItemGroup(handle: QGraphicsSceneH; group: QGraphicsItemGroupH); cdecl; external QtIntf name 'QGraphicsScene_destroyItemGroup';
procedure QGraphicsScene_addItem(handle: QGraphicsSceneH; item: QGraphicsItemH); cdecl; external QtIntf name 'QGraphicsScene_addItem';
function QGraphicsScene_addEllipse(handle: QGraphicsSceneH; rect: QRectFH; pen: QPenH = nil; brush: QBrushH = nil): QGraphicsEllipseItemH; cdecl; external QtIntf name 'QGraphicsScene_addEllipse';
function QGraphicsScene_addLine(handle: QGraphicsSceneH; line: QLineFH; pen: QPenH = nil): QGraphicsLineItemH; cdecl; external QtIntf name 'QGraphicsScene_addLine';
function QGraphicsScene_addPath(handle: QGraphicsSceneH; path: QPainterPathH; pen: QPenH = nil; brush: QBrushH = nil): QGraphicsPathItemH; cdecl; external QtIntf name 'QGraphicsScene_addPath';
function QGraphicsScene_addPixmap(handle: QGraphicsSceneH; pixmap: QPixmapH): QGraphicsPixmapItemH; cdecl; external QtIntf name 'QGraphicsScene_addPixmap';
function QGraphicsScene_addPolygon(handle: QGraphicsSceneH; polygon: QPolygonFH; pen: QPenH = nil; brush: QBrushH = nil): QGraphicsPolygonItemH; cdecl; external QtIntf name 'QGraphicsScene_addPolygon';
function QGraphicsScene_addRect(handle: QGraphicsSceneH; rect: QRectFH; pen: QPenH = nil; brush: QBrushH = nil): QGraphicsRectItemH; cdecl; external QtIntf name 'QGraphicsScene_addRect';
function QGraphicsScene_addText(handle: QGraphicsSceneH; text: PWideString; font: QFontH = nil): QGraphicsTextItemH; cdecl; external QtIntf name 'QGraphicsScene_addText';
procedure QGraphicsScene_removeItem(handle: QGraphicsSceneH; item: QGraphicsItemH); cdecl; external QtIntf name 'QGraphicsScene_removeItem';
function QGraphicsScene_focusItem(handle: QGraphicsSceneH): QGraphicsItemH; cdecl; external QtIntf name 'QGraphicsScene_focusItem';
procedure QGraphicsScene_setFocusItem(handle: QGraphicsSceneH; item: QGraphicsItemH; focusReason: QtFocusReason = QtOtherFocusReason); cdecl; external QtIntf name 'QGraphicsScene_setFocusItem';
function QGraphicsScene_hasFocus(handle: QGraphicsSceneH): Boolean; cdecl; external QtIntf name 'QGraphicsScene_hasFocus';
procedure QGraphicsScene_setFocus(handle: QGraphicsSceneH; focusReason: QtFocusReason = QtOtherFocusReason); cdecl; external QtIntf name 'QGraphicsScene_setFocus';
procedure QGraphicsScene_clearFocus(handle: QGraphicsSceneH); cdecl; external QtIntf name 'QGraphicsScene_clearFocus';
function QGraphicsScene_mouseGrabberItem(handle: QGraphicsSceneH): QGraphicsItemH; cdecl; external QtIntf name 'QGraphicsScene_mouseGrabberItem';
procedure QGraphicsScene_backgroundBrush(handle: QGraphicsSceneH; retval: QBrushH); cdecl; external QtIntf name 'QGraphicsScene_backgroundBrush';
procedure QGraphicsScene_setBackgroundBrush(handle: QGraphicsSceneH; brush: QBrushH); cdecl; external QtIntf name 'QGraphicsScene_setBackgroundBrush';
procedure QGraphicsScene_foregroundBrush(handle: QGraphicsSceneH; retval: QBrushH); cdecl; external QtIntf name 'QGraphicsScene_foregroundBrush';
procedure QGraphicsScene_setForegroundBrush(handle: QGraphicsSceneH; brush: QBrushH); cdecl; external QtIntf name 'QGraphicsScene_setForegroundBrush';
procedure QGraphicsScene_inputMethodQuery(handle: QGraphicsSceneH; retval: QVariantH; query: QtInputMethodQuery); cdecl; external QtIntf name 'QGraphicsScene_inputMethodQuery';
procedure QGraphicsScene_update(handle: QGraphicsSceneH; rect: QRectFH = nil); cdecl; external QtIntf name 'QGraphicsScene_update';
procedure QGraphicsScene_advance(handle: QGraphicsSceneH); cdecl; external QtIntf name 'QGraphicsScene_advance';


type
  QGraphicsScene_sceneRectChanged_Event = procedure (rect: QRectFH) of object cdecl;



type
  QGraphicsViewViewportAnchor = ( // QGraphicsView::ViewportAnchor (1)
    QGraphicsViewNoAnchor, QGraphicsViewAnchorViewCenter, QGraphicsViewAnchorUnderMouse );

  QGraphicsViewDragMode = ( // QGraphicsView::DragMode (1)
    QGraphicsViewNoDrag, QGraphicsViewScrollHandDrag, QGraphicsViewRubberBandDrag );

type
  QGraphicsViewCacheModeFlag = cardinal; // QGraphicsView::CacheModeFlag
  QGraphicsViewCacheMode = QGraphicsViewCacheModeFlag; //QFlags<> (3)
const
  QGraphicsViewCacheNone =   $0;
  QGraphicsViewCacheBackground =   $1;

function QGraphicsView_create(parent: QWidgetH = nil): QGraphicsViewH; overload; cdecl; external QtIntf name 'QGraphicsView_create';
procedure QGraphicsView_destroy(handle: QGraphicsViewH); cdecl; external QtIntf name 'QGraphicsView_destroy'; 
function QGraphicsView_create(scene: QGraphicsSceneH; parent: QWidgetH = nil): QGraphicsViewH; overload; cdecl; external QtIntf name 'QGraphicsView_create2';
procedure QGraphicsView_sizeHint(handle: QGraphicsViewH; retval: PSize); cdecl; external QtIntf name 'QGraphicsView_sizeHint';
function QGraphicsView_renderHints(handle: QGraphicsViewH): QPainterRenderHints; cdecl; external QtIntf name 'QGraphicsView_renderHints';
procedure QGraphicsView_setRenderHint(handle: QGraphicsViewH; hint: QPainterRenderHint; enabled: Boolean = True); cdecl; external QtIntf name 'QGraphicsView_setRenderHint';
procedure QGraphicsView_setRenderHints(handle: QGraphicsViewH; hints: QPainterRenderHints); cdecl; external QtIntf name 'QGraphicsView_setRenderHints';
function QGraphicsView_alignment(handle: QGraphicsViewH): QtAlignment; cdecl; external QtIntf name 'QGraphicsView_alignment';
procedure QGraphicsView_setAlignment(handle: QGraphicsViewH; alignment: QtAlignment); cdecl; external QtIntf name 'QGraphicsView_setAlignment';
function QGraphicsView_transformationAnchor(handle: QGraphicsViewH): QGraphicsViewViewportAnchor; cdecl; external QtIntf name 'QGraphicsView_transformationAnchor';
procedure QGraphicsView_setTransformationAnchor(handle: QGraphicsViewH; anchor: QGraphicsViewViewportAnchor); cdecl; external QtIntf name 'QGraphicsView_setTransformationAnchor';
function QGraphicsView_resizeAnchor(handle: QGraphicsViewH): QGraphicsViewViewportAnchor; cdecl; external QtIntf name 'QGraphicsView_resizeAnchor';
procedure QGraphicsView_setResizeAnchor(handle: QGraphicsViewH; anchor: QGraphicsViewViewportAnchor); cdecl; external QtIntf name 'QGraphicsView_setResizeAnchor';
function QGraphicsView_dragMode(handle: QGraphicsViewH): QGraphicsViewDragMode; cdecl; external QtIntf name 'QGraphicsView_dragMode';
procedure QGraphicsView_setDragMode(handle: QGraphicsViewH; mode: QGraphicsViewDragMode); cdecl; external QtIntf name 'QGraphicsView_setDragMode';
function QGraphicsView_cacheMode(handle: QGraphicsViewH): QGraphicsViewCacheMode; cdecl; external QtIntf name 'QGraphicsView_cacheMode';
procedure QGraphicsView_setCacheMode(handle: QGraphicsViewH; mode: QGraphicsViewCacheMode); cdecl; external QtIntf name 'QGraphicsView_setCacheMode';
procedure QGraphicsView_resetCachedContent(handle: QGraphicsViewH); cdecl; external QtIntf name 'QGraphicsView_resetCachedContent';
function QGraphicsView_isInteractive(handle: QGraphicsViewH): Boolean; cdecl; external QtIntf name 'QGraphicsView_isInteractive';
procedure QGraphicsView_setInteractive(handle: QGraphicsViewH; allowed: Boolean); cdecl; external QtIntf name 'QGraphicsView_setInteractive';
function QGraphicsView_scene(handle: QGraphicsViewH): QGraphicsSceneH; cdecl; external QtIntf name 'QGraphicsView_scene';
procedure QGraphicsView_setScene(handle: QGraphicsViewH; scene: QGraphicsSceneH); cdecl; external QtIntf name 'QGraphicsView_setScene';
procedure QGraphicsView_sceneRect(handle: QGraphicsViewH; retval: QRectFH); cdecl; external QtIntf name 'QGraphicsView_sceneRect';
procedure QGraphicsView_setSceneRect(handle: QGraphicsViewH; rect: QRectFH); overload; cdecl; external QtIntf name 'QGraphicsView_setSceneRect';
procedure QGraphicsView_setSceneRect(handle: QGraphicsViewH; x: Double; y: Double; w: Double; h: Double); overload; cdecl; external QtIntf name 'QGraphicsView_setSceneRect2';
procedure QGraphicsView_matrix(handle: QGraphicsViewH; retval: QMatrixH); cdecl; external QtIntf name 'QGraphicsView_matrix';
procedure QGraphicsView_setMatrix(handle: QGraphicsViewH; matrix: QMatrixH; combine: Boolean = False); cdecl; external QtIntf name 'QGraphicsView_setMatrix';
procedure QGraphicsView_resetMatrix(handle: QGraphicsViewH); cdecl; external QtIntf name 'QGraphicsView_resetMatrix';
procedure QGraphicsView_rotate(handle: QGraphicsViewH; angle: Double); cdecl; external QtIntf name 'QGraphicsView_rotate';
procedure QGraphicsView_scale(handle: QGraphicsViewH; sx: Double; sy: Double); cdecl; external QtIntf name 'QGraphicsView_scale';
procedure QGraphicsView_shear(handle: QGraphicsViewH; sh: Double; sv: Double); cdecl; external QtIntf name 'QGraphicsView_shear';
procedure QGraphicsView_translate(handle: QGraphicsViewH; dx: Double; dy: Double); cdecl; external QtIntf name 'QGraphicsView_translate';
procedure QGraphicsView_centerOn(handle: QGraphicsViewH; pos: QPointFH); overload; cdecl; external QtIntf name 'QGraphicsView_centerOn';
procedure QGraphicsView_centerOn(handle: QGraphicsViewH; x: Double; y: Double); overload; cdecl; external QtIntf name 'QGraphicsView_centerOn2';
procedure QGraphicsView_centerOn(handle: QGraphicsViewH; item: QGraphicsItemH); overload; cdecl; external QtIntf name 'QGraphicsView_centerOn3';
procedure QGraphicsView_ensureVisible(handle: QGraphicsViewH; rect: QRectFH; xmargin: Integer = 50; ymargin: Integer = 50); overload; cdecl; external QtIntf name 'QGraphicsView_ensureVisible';
procedure QGraphicsView_ensureVisible(handle: QGraphicsViewH; x: Double; y: Double; w: Double; h: Double; xmargin: Integer = 50; ymargin: Integer = 50); overload; cdecl; external QtIntf name 'QGraphicsView_ensureVisible2';
procedure QGraphicsView_ensureVisible(handle: QGraphicsViewH; item: QGraphicsItemH; xmargin: Integer = 50; ymargin: Integer = 50); overload; cdecl; external QtIntf name 'QGraphicsView_ensureVisible3';
procedure QGraphicsView_fitInView(handle: QGraphicsViewH; rect: QRectFH; aspectRadioMode: QtAspectRatioMode = QtIgnoreAspectRatio); overload; cdecl; external QtIntf name 'QGraphicsView_fitInView';
procedure QGraphicsView_fitInView(handle: QGraphicsViewH; x: Double; y: Double; w: Double; h: Double; aspectRadioMode: QtAspectRatioMode = QtIgnoreAspectRatio); overload; cdecl; external QtIntf name 'QGraphicsView_fitInView2';
procedure QGraphicsView_fitInView(handle: QGraphicsViewH; item: QGraphicsItemH; aspectRadioMode: QtAspectRatioMode = QtIgnoreAspectRatio); overload; cdecl; external QtIntf name 'QGraphicsView_fitInView3';
procedure QGraphicsView_render(handle: QGraphicsViewH; painter: QPainterH; target: QRectFH = nil; source: PRect = nil; aspectRatioMode: QtAspectRatioMode = QtKeepAspectRatio); cdecl; external QtIntf name 'QGraphicsView_render';
function QGraphicsView_itemAt(handle: QGraphicsViewH; pos: PQtPoint): QGraphicsItemH; overload; cdecl; external QtIntf name 'QGraphicsView_itemAt';
function QGraphicsView_itemAt(handle: QGraphicsViewH; x: Integer; y: Integer): QGraphicsItemH; overload; cdecl; external QtIntf name 'QGraphicsView_itemAt2';
procedure QGraphicsView_mapToScene(handle: QGraphicsViewH; retval: QPointFH; point: PQtPoint); overload; cdecl; external QtIntf name 'QGraphicsView_mapToScene';
procedure QGraphicsView_mapToScene(handle: QGraphicsViewH; retval: QPolygonFH; rect: PRect); overload; cdecl; external QtIntf name 'QGraphicsView_mapToScene2';
procedure QGraphicsView_mapToScene(handle: QGraphicsViewH; retval: QPolygonFH; polygon: QPolygonH); overload; cdecl; external QtIntf name 'QGraphicsView_mapToScene3';
procedure QGraphicsView_mapToScene(handle: QGraphicsViewH; retval: QPainterPathH; path: QPainterPathH); overload; cdecl; external QtIntf name 'QGraphicsView_mapToScene4';
procedure QGraphicsView_mapFromScene(handle: QGraphicsViewH; retval: PQtPoint; point: QPointFH); overload; cdecl; external QtIntf name 'QGraphicsView_mapFromScene';
procedure QGraphicsView_mapFromScene(handle: QGraphicsViewH; retval: QPolygonH; rect: QRectFH); overload; cdecl; external QtIntf name 'QGraphicsView_mapFromScene2';
procedure QGraphicsView_mapFromScene(handle: QGraphicsViewH; retval: QPolygonH; polygon: QPolygonFH); overload; cdecl; external QtIntf name 'QGraphicsView_mapFromScene3';
procedure QGraphicsView_mapFromScene(handle: QGraphicsViewH; retval: QPainterPathH; path: QPainterPathH); overload; cdecl; external QtIntf name 'QGraphicsView_mapFromScene4';
procedure QGraphicsView_mapToScene(handle: QGraphicsViewH; retval: QPointFH; x: Integer; y: Integer); overload; cdecl; external QtIntf name 'QGraphicsView_mapToScene5';
procedure QGraphicsView_mapToScene(handle: QGraphicsViewH; retval: QPolygonFH; x: Integer; y: Integer; w: Integer; h: Integer); overload; cdecl; external QtIntf name 'QGraphicsView_mapToScene6';
procedure QGraphicsView_mapFromScene(handle: QGraphicsViewH; retval: PQtPoint; x: Double; y: Double); overload; cdecl; external QtIntf name 'QGraphicsView_mapFromScene5';
procedure QGraphicsView_mapFromScene(handle: QGraphicsViewH; retval: QPolygonH; x: Double; y: Double; w: Double; h: Double); overload; cdecl; external QtIntf name 'QGraphicsView_mapFromScene6';
procedure QGraphicsView_inputMethodQuery(handle: QGraphicsViewH; retval: QVariantH; query: QtInputMethodQuery); cdecl; external QtIntf name 'QGraphicsView_inputMethodQuery';
procedure QGraphicsView_backgroundBrush(handle: QGraphicsViewH; retval: QBrushH); cdecl; external QtIntf name 'QGraphicsView_backgroundBrush';
procedure QGraphicsView_setBackgroundBrush(handle: QGraphicsViewH; brush: QBrushH); cdecl; external QtIntf name 'QGraphicsView_setBackgroundBrush';
procedure QGraphicsView_foregroundBrush(handle: QGraphicsViewH; retval: QBrushH); cdecl; external QtIntf name 'QGraphicsView_foregroundBrush';
procedure QGraphicsView_setForegroundBrush(handle: QGraphicsViewH; brush: QBrushH); cdecl; external QtIntf name 'QGraphicsView_setForegroundBrush';
procedure QGraphicsView_updateSceneRect(handle: QGraphicsViewH; rect: QRectFH); cdecl; external QtIntf name 'QGraphicsView_updateSceneRect';

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


function QIODevice_openMode(handle: QIODeviceH): QIODeviceOpenMode; cdecl; external QtIntf name 'QIODevice_openMode';
procedure QIODevice_setTextModeEnabled(handle: QIODeviceH; enabled: Boolean); cdecl; external QtIntf name 'QIODevice_setTextModeEnabled';
function QIODevice_isTextModeEnabled(handle: QIODeviceH): Boolean; cdecl; external QtIntf name 'QIODevice_isTextModeEnabled';
function QIODevice_isOpen(handle: QIODeviceH): Boolean; cdecl; external QtIntf name 'QIODevice_isOpen';
function QIODevice_isReadable(handle: QIODeviceH): Boolean; cdecl; external QtIntf name 'QIODevice_isReadable';
function QIODevice_isWritable(handle: QIODeviceH): Boolean; cdecl; external QtIntf name 'QIODevice_isWritable';
function QIODevice_isSequential(handle: QIODeviceH): Boolean; cdecl; external QtIntf name 'QIODevice_isSequential';
function QIODevice_open(handle: QIODeviceH; mode: QIODeviceOpenMode): Boolean; cdecl; external QtIntf name 'QIODevice_open';
procedure QIODevice_close(handle: QIODeviceH); cdecl; external QtIntf name 'QIODevice_close';
function QIODevice_pos(handle: QIODeviceH): int64; cdecl; external QtIntf name 'QIODevice_pos';
function QIODevice_size(handle: QIODeviceH): int64; cdecl; external QtIntf name 'QIODevice_size';
function QIODevice_seek(handle: QIODeviceH; pos: int64): Boolean; cdecl; external QtIntf name 'QIODevice_seek';
function QIODevice_atEnd(handle: QIODeviceH): Boolean; cdecl; external QtIntf name 'QIODevice_atEnd';
function QIODevice_reset(handle: QIODeviceH): Boolean; cdecl; external QtIntf name 'QIODevice_reset';
function QIODevice_bytesAvailable(handle: QIODeviceH): int64; cdecl; external QtIntf name 'QIODevice_bytesAvailable';
function QIODevice_bytesToWrite(handle: QIODeviceH): int64; cdecl; external QtIntf name 'QIODevice_bytesToWrite';
function QIODevice_read(handle: QIODeviceH; data: PAnsiChar; maxlen: int64): int64; overload; cdecl; external QtIntf name 'QIODevice_read';
procedure QIODevice_read(handle: QIODeviceH; retval: QByteArrayH; maxlen: int64); overload; cdecl; external QtIntf name 'QIODevice_read2';
procedure QIODevice_readAll(handle: QIODeviceH; retval: QByteArrayH); cdecl; external QtIntf name 'QIODevice_readAll';
function QIODevice_readLine(handle: QIODeviceH; data: PAnsiChar; maxlen: int64): int64; overload; cdecl; external QtIntf name 'QIODevice_readLine';
procedure QIODevice_readLine(handle: QIODeviceH; retval: QByteArrayH; maxlen: int64 = 0); overload; cdecl; external QtIntf name 'QIODevice_readLine2';
function QIODevice_canReadLine(handle: QIODeviceH): Boolean; cdecl; external QtIntf name 'QIODevice_canReadLine';
function QIODevice_write(handle: QIODeviceH; data: PAnsiChar; len: int64): int64; overload; cdecl; external QtIntf name 'QIODevice_write';
function QIODevice_write(handle: QIODeviceH; data: QByteArrayH): int64; overload; cdecl; external QtIntf name 'QIODevice_write2';
function QIODevice_peek(handle: QIODeviceH; data: PAnsiChar; maxlen: int64): int64; overload; cdecl; external QtIntf name 'QIODevice_peek';
procedure QIODevice_peek(handle: QIODeviceH; retval: QByteArrayH; maxlen: int64); overload; cdecl; external QtIntf name 'QIODevice_peek2';
function QIODevice_waitForReadyRead(handle: QIODeviceH; msecs: Integer): Boolean; cdecl; external QtIntf name 'QIODevice_waitForReadyRead';
function QIODevice_waitForBytesWritten(handle: QIODeviceH; msecs: Integer): Boolean; cdecl; external QtIntf name 'QIODevice_waitForBytesWritten';
procedure QIODevice_ungetChar(handle: QIODeviceH; c: char); cdecl; external QtIntf name 'QIODevice_ungetChar';
function QIODevice_putChar(handle: QIODeviceH; c: char): Boolean; cdecl; external QtIntf name 'QIODevice_putChar';
function QIODevice_getChar(handle: QIODeviceH; c: PAnsiChar): Boolean; cdecl; external QtIntf name 'QIODevice_getChar';
procedure QIODevice_errorString(handle: QIODeviceH; retval: PWideString); cdecl; external QtIntf name 'QIODevice_errorString';


type
  QIODevice_readyRead_Event = procedure () of object cdecl;
  QIODevice_bytesWritten_Event = procedure (bytes: int64) of object cdecl;
  QIODevice_aboutToClose_Event = procedure () of object cdecl;


function QEvent_hook_create(handle: QObjectH): QEvent_hookH; cdecl; external QtIntf name 'QEvent_hook_create';
procedure QEvent_hook_destroy(handle: QEvent_hookH); cdecl; external QtIntf name 'QEvent_hook_destroy'; 

function QTimerEvent_hook_create(handle: QObjectH): QTimerEvent_hookH; cdecl; external QtIntf name 'QTimerEvent_hook_create';
procedure QTimerEvent_hook_destroy(handle: QTimerEvent_hookH); cdecl; external QtIntf name 'QTimerEvent_hook_destroy'; 

function QChildEvent_hook_create(handle: QObjectH): QChildEvent_hookH; cdecl; external QtIntf name 'QChildEvent_hook_create';
procedure QChildEvent_hook_destroy(handle: QChildEvent_hookH); cdecl; external QtIntf name 'QChildEvent_hook_destroy'; 

function QDynamicPropertyChangeEvent_hook_create(handle: QObjectH): QDynamicPropertyChangeEvent_hookH; cdecl; external QtIntf name 'QDynamicPropertyChangeEvent_hook_create';
procedure QDynamicPropertyChangeEvent_hook_destroy(handle: QDynamicPropertyChangeEvent_hookH); cdecl; external QtIntf name 'QDynamicPropertyChangeEvent_hook_destroy'; 

function QEventLoop_hook_create(handle: QObjectH): QEventLoop_hookH; cdecl; external QtIntf name 'QEventLoop_hook_create';
procedure QEventLoop_hook_destroy(handle: QEventLoop_hookH); cdecl; external QtIntf name 'QEventLoop_hook_destroy'; 

function QCoreApplication_hook_create(handle: QObjectH): QCoreApplication_hookH; cdecl; external QtIntf name 'QCoreApplication_hook_create';
procedure QCoreApplication_hook_destroy(handle: QCoreApplication_hookH); cdecl; external QtIntf name 'QCoreApplication_hook_destroy'; 
procedure QCoreApplication_hook_hook_aboutToQuit(handle: QCoreApplication_hookH; hook: QHookH); cdecl; external QtIntf name 'QCoreApplication_hook_hook_aboutToQuit';
procedure QCoreApplication_hook_hook_unixSignal(handle: QCoreApplication_hookH; hook: QHookH); cdecl; external QtIntf name 'QCoreApplication_hook_hook_unixSignal';

function QTimer_hook_create(handle: QObjectH): QTimer_hookH; cdecl; external QtIntf name 'QTimer_hook_create';
procedure QTimer_hook_destroy(handle: QTimer_hookH); cdecl; external QtIntf name 'QTimer_hook_destroy'; 
procedure QTimer_hook_hook_timeout(handle: QTimer_hookH; hook: QHookH); cdecl; external QtIntf name 'QTimer_hook_hook_timeout';

function QModelIndex_hook_create(handle: QObjectH): QModelIndex_hookH; cdecl; external QtIntf name 'QModelIndex_hook_create';
procedure QModelIndex_hook_destroy(handle: QModelIndex_hookH); cdecl; external QtIntf name 'QModelIndex_hook_destroy'; 

function QPersistentModelIndex_hook_create(handle: QObjectH): QPersistentModelIndex_hookH; cdecl; external QtIntf name 'QPersistentModelIndex_hook_create';
procedure QPersistentModelIndex_hook_destroy(handle: QPersistentModelIndex_hookH); cdecl; external QtIntf name 'QPersistentModelIndex_hook_destroy'; 

function QAbstractItemModel_hook_create(handle: QObjectH): QAbstractItemModel_hookH; cdecl; external QtIntf name 'QAbstractItemModel_hook_create';
procedure QAbstractItemModel_hook_destroy(handle: QAbstractItemModel_hookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_destroy'; 
procedure QAbstractItemModel_hook_hook_dataChanged(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_dataChanged';
procedure QAbstractItemModel_hook_hook_headerDataChanged(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_headerDataChanged';
procedure QAbstractItemModel_hook_hook_layoutChanged(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_layoutChanged';
procedure QAbstractItemModel_hook_hook_layoutAboutToBeChanged(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_layoutAboutToBeChanged';
procedure QAbstractItemModel_hook_hook_rowsAboutToBeInserted(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_rowsAboutToBeInserted';
procedure QAbstractItemModel_hook_hook_rowsInserted(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_rowsInserted';
procedure QAbstractItemModel_hook_hook_rowsAboutToBeRemoved(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_rowsAboutToBeRemoved';
procedure QAbstractItemModel_hook_hook_rowsRemoved(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_rowsRemoved';
procedure QAbstractItemModel_hook_hook_columnsAboutToBeInserted(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_columnsAboutToBeInserted';
procedure QAbstractItemModel_hook_hook_columnsInserted(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_columnsInserted';
procedure QAbstractItemModel_hook_hook_columnsAboutToBeRemoved(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_columnsAboutToBeRemoved';
procedure QAbstractItemModel_hook_hook_columnsRemoved(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_columnsRemoved';
procedure QAbstractItemModel_hook_hook_modelAboutToBeReset(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_modelAboutToBeReset';
procedure QAbstractItemModel_hook_hook_modelReset(handle: QAbstractItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemModel_hook_hook_modelReset';

function QAbstractTableModel_hook_create(handle: QObjectH): QAbstractTableModel_hookH; cdecl; external QtIntf name 'QAbstractTableModel_hook_create';
procedure QAbstractTableModel_hook_destroy(handle: QAbstractTableModel_hookH); cdecl; external QtIntf name 'QAbstractTableModel_hook_destroy'; 

function QAbstractListModel_hook_create(handle: QObjectH): QAbstractListModel_hookH; cdecl; external QtIntf name 'QAbstractListModel_hook_create';
procedure QAbstractListModel_hook_destroy(handle: QAbstractListModel_hookH); cdecl; external QtIntf name 'QAbstractListModel_hook_destroy'; 

function QApplication_hook_create(handle: QObjectH): QApplication_hookH; cdecl; external QtIntf name 'QApplication_hook_create';
procedure QApplication_hook_destroy(handle: QApplication_hookH); cdecl; external QtIntf name 'QApplication_hook_destroy'; 
procedure QApplication_hook_hook_lastWindowClosed(handle: QApplication_hookH; hook: QHookH); cdecl; external QtIntf name 'QApplication_hook_hook_lastWindowClosed';
procedure QApplication_hook_hook_focusChanged(handle: QApplication_hookH; hook: QHookH); cdecl; external QtIntf name 'QApplication_hook_hook_focusChanged';
procedure QApplication_hook_hook_commitDataRequest(handle: QApplication_hookH; hook: QHookH); cdecl; external QtIntf name 'QApplication_hook_hook_commitDataRequest';
procedure QApplication_hook_hook_saveStateRequest(handle: QApplication_hookH; hook: QHookH); cdecl; external QtIntf name 'QApplication_hook_hook_saveStateRequest';

function QWidget_hook_create(handle: QObjectH): QWidget_hookH; cdecl; external QtIntf name 'QWidget_hook_create';
procedure QWidget_hook_destroy(handle: QWidget_hookH); cdecl; external QtIntf name 'QWidget_hook_destroy'; 
procedure QWidget_hook_hook_customContextMenuRequested(handle: QWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QWidget_hook_hook_customContextMenuRequested';

function QAction_hook_create(handle: QObjectH): QAction_hookH; cdecl; external QtIntf name 'QAction_hook_create';
procedure QAction_hook_destroy(handle: QAction_hookH); cdecl; external QtIntf name 'QAction_hook_destroy'; 
procedure QAction_hook_hook_changed(handle: QAction_hookH; hook: QHookH); cdecl; external QtIntf name 'QAction_hook_hook_changed';
procedure QAction_hook_hook_triggered(handle: QAction_hookH; hook: QHookH); cdecl; external QtIntf name 'QAction_hook_hook_triggered';
procedure QAction_hook_hook_triggered2(handle: QAction_hookH; hook: QHookH); cdecl; external QtIntf name 'QAction_hook_hook_triggered2';
procedure QAction_hook_hook_hovered(handle: QAction_hookH; hook: QHookH); cdecl; external QtIntf name 'QAction_hook_hook_hovered';
procedure QAction_hook_hook_toggled(handle: QAction_hookH; hook: QHookH); cdecl; external QtIntf name 'QAction_hook_hook_toggled';

function QClipboard_hook_create(handle: QObjectH): QClipboard_hookH; cdecl; external QtIntf name 'QClipboard_hook_create';
procedure QClipboard_hook_destroy(handle: QClipboard_hookH); cdecl; external QtIntf name 'QClipboard_hook_destroy'; 
procedure QClipboard_hook_hook_changed(handle: QClipboard_hookH; hook: QHookH); cdecl; external QtIntf name 'QClipboard_hook_hook_changed';
procedure QClipboard_hook_hook_selectionChanged(handle: QClipboard_hookH; hook: QHookH); cdecl; external QtIntf name 'QClipboard_hook_hook_selectionChanged';
procedure QClipboard_hook_hook_findBufferChanged(handle: QClipboard_hookH; hook: QHookH); cdecl; external QtIntf name 'QClipboard_hook_hook_findBufferChanged';
procedure QClipboard_hook_hook_dataChanged(handle: QClipboard_hookH; hook: QHookH); cdecl; external QtIntf name 'QClipboard_hook_hook_dataChanged';

function QDesktopWidget_hook_create(handle: QObjectH): QDesktopWidget_hookH; cdecl; external QtIntf name 'QDesktopWidget_hook_create';
procedure QDesktopWidget_hook_destroy(handle: QDesktopWidget_hookH); cdecl; external QtIntf name 'QDesktopWidget_hook_destroy'; 
procedure QDesktopWidget_hook_hook_resized(handle: QDesktopWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QDesktopWidget_hook_hook_resized';
procedure QDesktopWidget_hook_hook_workAreaResized(handle: QDesktopWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QDesktopWidget_hook_hook_workAreaResized';

function QDrag_hook_create(handle: QObjectH): QDrag_hookH; cdecl; external QtIntf name 'QDrag_hook_create';
procedure QDrag_hook_destroy(handle: QDrag_hookH); cdecl; external QtIntf name 'QDrag_hook_destroy'; 
procedure QDrag_hook_hook_actionChanged(handle: QDrag_hookH; hook: QHookH); cdecl; external QtIntf name 'QDrag_hook_hook_actionChanged';
procedure QDrag_hook_hook_targetChanged(handle: QDrag_hookH; hook: QHookH); cdecl; external QtIntf name 'QDrag_hook_hook_targetChanged';

function QFrame_hook_create(handle: QObjectH): QFrame_hookH; cdecl; external QtIntf name 'QFrame_hook_create';
procedure QFrame_hook_destroy(handle: QFrame_hookH); cdecl; external QtIntf name 'QFrame_hook_destroy'; 

function QAbstractScrollArea_hook_create(handle: QObjectH): QAbstractScrollArea_hookH; cdecl; external QtIntf name 'QAbstractScrollArea_hook_create';
procedure QAbstractScrollArea_hook_destroy(handle: QAbstractScrollArea_hookH); cdecl; external QtIntf name 'QAbstractScrollArea_hook_destroy'; 

function QAbstractSlider_hook_create(handle: QObjectH): QAbstractSlider_hookH; cdecl; external QtIntf name 'QAbstractSlider_hook_create';
procedure QAbstractSlider_hook_destroy(handle: QAbstractSlider_hookH); cdecl; external QtIntf name 'QAbstractSlider_hook_destroy'; 
procedure QAbstractSlider_hook_hook_valueChanged(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractSlider_hook_hook_valueChanged';
procedure QAbstractSlider_hook_hook_sliderPressed(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractSlider_hook_hook_sliderPressed';
procedure QAbstractSlider_hook_hook_sliderMoved(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractSlider_hook_hook_sliderMoved';
procedure QAbstractSlider_hook_hook_sliderReleased(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractSlider_hook_hook_sliderReleased';
procedure QAbstractSlider_hook_hook_rangeChanged(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractSlider_hook_hook_rangeChanged';
procedure QAbstractSlider_hook_hook_actionTriggered(handle: QAbstractSlider_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractSlider_hook_hook_actionTriggered';

function QScrollBar_hook_create(handle: QObjectH): QScrollBar_hookH; cdecl; external QtIntf name 'QScrollBar_hook_create';
procedure QScrollBar_hook_destroy(handle: QScrollBar_hookH); cdecl; external QtIntf name 'QScrollBar_hook_destroy'; 

function QMenu_hook_create(handle: QObjectH): QMenu_hookH; cdecl; external QtIntf name 'QMenu_hook_create';
procedure QMenu_hook_destroy(handle: QMenu_hookH); cdecl; external QtIntf name 'QMenu_hook_destroy'; 
procedure QMenu_hook_hook_aboutToShow(handle: QMenu_hookH; hook: QHookH); cdecl; external QtIntf name 'QMenu_hook_hook_aboutToShow';
procedure QMenu_hook_hook_aboutToHide(handle: QMenu_hookH; hook: QHookH); cdecl; external QtIntf name 'QMenu_hook_hook_aboutToHide';
procedure QMenu_hook_hook_triggered(handle: QMenu_hookH; hook: QHookH); cdecl; external QtIntf name 'QMenu_hook_hook_triggered';
procedure QMenu_hook_hook_hovered(handle: QMenu_hookH; hook: QHookH); cdecl; external QtIntf name 'QMenu_hook_hook_hovered';

function QMenuBar_hook_create(handle: QObjectH): QMenuBar_hookH; cdecl; external QtIntf name 'QMenuBar_hook_create';
procedure QMenuBar_hook_destroy(handle: QMenuBar_hookH); cdecl; external QtIntf name 'QMenuBar_hook_destroy'; 
procedure QMenuBar_hook_hook_triggered(handle: QMenuBar_hookH; hook: QHookH); cdecl; external QtIntf name 'QMenuBar_hook_hook_triggered';
procedure QMenuBar_hook_hook_hovered(handle: QMenuBar_hookH; hook: QHookH); cdecl; external QtIntf name 'QMenuBar_hook_hook_hovered';

function QButtonGroup_hook_create(handle: QObjectH): QButtonGroup_hookH; cdecl; external QtIntf name 'QButtonGroup_hook_create';
procedure QButtonGroup_hook_destroy(handle: QButtonGroup_hookH); cdecl; external QtIntf name 'QButtonGroup_hook_destroy'; 
procedure QButtonGroup_hook_hook_buttonClicked(handle: QButtonGroup_hookH; hook: QHookH); cdecl; external QtIntf name 'QButtonGroup_hook_hook_buttonClicked';
procedure QButtonGroup_hook_hook_buttonClicked2(handle: QButtonGroup_hookH; hook: QHookH); cdecl; external QtIntf name 'QButtonGroup_hook_hook_buttonClicked2';
procedure QButtonGroup_hook_hook_buttonPressed(handle: QButtonGroup_hookH; hook: QHookH); cdecl; external QtIntf name 'QButtonGroup_hook_hook_buttonPressed';
procedure QButtonGroup_hook_hook_buttonPressed2(handle: QButtonGroup_hookH; hook: QHookH); cdecl; external QtIntf name 'QButtonGroup_hook_hook_buttonPressed2';
procedure QButtonGroup_hook_hook_buttonReleased(handle: QButtonGroup_hookH; hook: QHookH); cdecl; external QtIntf name 'QButtonGroup_hook_hook_buttonReleased';
procedure QButtonGroup_hook_hook_buttonReleased2(handle: QButtonGroup_hookH; hook: QHookH); cdecl; external QtIntf name 'QButtonGroup_hook_hook_buttonReleased2';

function QAbstractButton_hook_create(handle: QObjectH): QAbstractButton_hookH; cdecl; external QtIntf name 'QAbstractButton_hook_create';
procedure QAbstractButton_hook_destroy(handle: QAbstractButton_hookH); cdecl; external QtIntf name 'QAbstractButton_hook_destroy'; 
procedure QAbstractButton_hook_hook_pressed(handle: QAbstractButton_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractButton_hook_hook_pressed';
procedure QAbstractButton_hook_hook_released(handle: QAbstractButton_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractButton_hook_hook_released';
procedure QAbstractButton_hook_hook_clicked(handle: QAbstractButton_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractButton_hook_hook_clicked';
procedure QAbstractButton_hook_hook_clicked2(handle: QAbstractButton_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractButton_hook_hook_clicked2';
procedure QAbstractButton_hook_hook_toggled(handle: QAbstractButton_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractButton_hook_hook_toggled';

function QPushButton_hook_create(handle: QObjectH): QPushButton_hookH; cdecl; external QtIntf name 'QPushButton_hook_create';
procedure QPushButton_hook_destroy(handle: QPushButton_hookH); cdecl; external QtIntf name 'QPushButton_hook_destroy'; 

function QLineEdit_hook_create(handle: QObjectH): QLineEdit_hookH; cdecl; external QtIntf name 'QLineEdit_hook_create';
procedure QLineEdit_hook_destroy(handle: QLineEdit_hookH); cdecl; external QtIntf name 'QLineEdit_hook_destroy'; 
procedure QLineEdit_hook_hook_textChanged(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QLineEdit_hook_hook_textChanged';
procedure QLineEdit_hook_hook_textEdited(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QLineEdit_hook_hook_textEdited';
procedure QLineEdit_hook_hook_cursorPositionChanged(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QLineEdit_hook_hook_cursorPositionChanged';
procedure QLineEdit_hook_hook_returnPressed(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QLineEdit_hook_hook_returnPressed';
procedure QLineEdit_hook_hook_editingFinished(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QLineEdit_hook_hook_editingFinished';
procedure QLineEdit_hook_hook_selectionChanged(handle: QLineEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QLineEdit_hook_hook_selectionChanged';

function QTextEdit_hook_create(handle: QObjectH): QTextEdit_hookH; cdecl; external QtIntf name 'QTextEdit_hook_create';
procedure QTextEdit_hook_destroy(handle: QTextEdit_hookH); cdecl; external QtIntf name 'QTextEdit_hook_destroy'; 
procedure QTextEdit_hook_hook_textChanged(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextEdit_hook_hook_textChanged';
procedure QTextEdit_hook_hook_undoAvailable(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextEdit_hook_hook_undoAvailable';
procedure QTextEdit_hook_hook_redoAvailable(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextEdit_hook_hook_redoAvailable';
procedure QTextEdit_hook_hook_currentCharFormatChanged(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextEdit_hook_hook_currentCharFormatChanged';
procedure QTextEdit_hook_hook_copyAvailable(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextEdit_hook_hook_copyAvailable';
procedure QTextEdit_hook_hook_selectionChanged(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextEdit_hook_hook_selectionChanged';
procedure QTextEdit_hook_hook_cursorPositionChanged(handle: QTextEdit_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextEdit_hook_hook_cursorPositionChanged';

function QMainWindow_hook_create(handle: QObjectH): QMainWindow_hookH; cdecl; external QtIntf name 'QMainWindow_hook_create';
procedure QMainWindow_hook_destroy(handle: QMainWindow_hookH); cdecl; external QtIntf name 'QMainWindow_hook_destroy'; 
procedure QMainWindow_hook_hook_iconSizeChanged(handle: QMainWindow_hookH; hook: QHookH); cdecl; external QtIntf name 'QMainWindow_hook_hook_iconSizeChanged';
procedure QMainWindow_hook_hook_toolButtonStyleChanged(handle: QMainWindow_hookH; hook: QHookH); cdecl; external QtIntf name 'QMainWindow_hook_hook_toolButtonStyleChanged';

function QToolBar_hook_create(handle: QObjectH): QToolBar_hookH; cdecl; external QtIntf name 'QToolBar_hook_create';
procedure QToolBar_hook_destroy(handle: QToolBar_hookH); cdecl; external QtIntf name 'QToolBar_hook_destroy'; 
procedure QToolBar_hook_hook_actionTriggered(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtIntf name 'QToolBar_hook_hook_actionTriggered';
procedure QToolBar_hook_hook_movableChanged(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtIntf name 'QToolBar_hook_hook_movableChanged';
procedure QToolBar_hook_hook_allowedAreasChanged(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtIntf name 'QToolBar_hook_hook_allowedAreasChanged';
procedure QToolBar_hook_hook_orientationChanged(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtIntf name 'QToolBar_hook_hook_orientationChanged';
procedure QToolBar_hook_hook_iconSizeChanged(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtIntf name 'QToolBar_hook_hook_iconSizeChanged';
procedure QToolBar_hook_hook_toolButtonStyleChanged(handle: QToolBar_hookH; hook: QHookH); cdecl; external QtIntf name 'QToolBar_hook_hook_toolButtonStyleChanged';

function QLCDNumber_hook_create(handle: QObjectH): QLCDNumber_hookH; cdecl; external QtIntf name 'QLCDNumber_hook_create';
procedure QLCDNumber_hook_destroy(handle: QLCDNumber_hookH); cdecl; external QtIntf name 'QLCDNumber_hook_destroy'; 
procedure QLCDNumber_hook_hook_overflow(handle: QLCDNumber_hookH; hook: QHookH); cdecl; external QtIntf name 'QLCDNumber_hook_hook_overflow';

function QAbstractSpinBox_hook_create(handle: QObjectH): QAbstractSpinBox_hookH; cdecl; external QtIntf name 'QAbstractSpinBox_hook_create';
procedure QAbstractSpinBox_hook_destroy(handle: QAbstractSpinBox_hookH); cdecl; external QtIntf name 'QAbstractSpinBox_hook_destroy'; 
procedure QAbstractSpinBox_hook_hook_editingFinished(handle: QAbstractSpinBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractSpinBox_hook_hook_editingFinished';

function QSpinBox_hook_create(handle: QObjectH): QSpinBox_hookH; cdecl; external QtIntf name 'QSpinBox_hook_create';
procedure QSpinBox_hook_destroy(handle: QSpinBox_hookH); cdecl; external QtIntf name 'QSpinBox_hook_destroy'; 
procedure QSpinBox_hook_hook_valueChanged(handle: QSpinBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QSpinBox_hook_hook_valueChanged';
procedure QSpinBox_hook_hook_valueChanged2(handle: QSpinBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QSpinBox_hook_hook_valueChanged2';

function QDoubleSpinBox_hook_create(handle: QObjectH): QDoubleSpinBox_hookH; cdecl; external QtIntf name 'QDoubleSpinBox_hook_create';
procedure QDoubleSpinBox_hook_destroy(handle: QDoubleSpinBox_hookH); cdecl; external QtIntf name 'QDoubleSpinBox_hook_destroy'; 
procedure QDoubleSpinBox_hook_hook_valueChanged(handle: QDoubleSpinBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QDoubleSpinBox_hook_hook_valueChanged';
procedure QDoubleSpinBox_hook_hook_valueChanged2(handle: QDoubleSpinBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QDoubleSpinBox_hook_hook_valueChanged2';

function QSplitter_hook_create(handle: QObjectH): QSplitter_hookH; cdecl; external QtIntf name 'QSplitter_hook_create';
procedure QSplitter_hook_destroy(handle: QSplitter_hookH); cdecl; external QtIntf name 'QSplitter_hook_destroy'; 
procedure QSplitter_hook_hook_splitterMoved(handle: QSplitter_hookH; hook: QHookH); cdecl; external QtIntf name 'QSplitter_hook_hook_splitterMoved';

function QSplitterHandle_hook_create(handle: QObjectH): QSplitterHandle_hookH; cdecl; external QtIntf name 'QSplitterHandle_hook_create';
procedure QSplitterHandle_hook_destroy(handle: QSplitterHandle_hookH); cdecl; external QtIntf name 'QSplitterHandle_hook_destroy'; 

function QWorkspace_hook_create(handle: QObjectH): QWorkspace_hookH; cdecl; external QtIntf name 'QWorkspace_hook_create';
procedure QWorkspace_hook_destroy(handle: QWorkspace_hookH); cdecl; external QtIntf name 'QWorkspace_hook_destroy'; 
procedure QWorkspace_hook_hook_windowActivated(handle: QWorkspace_hookH; hook: QHookH); cdecl; external QtIntf name 'QWorkspace_hook_hook_windowActivated';

function QComboBox_hook_create(handle: QObjectH): QComboBox_hookH; cdecl; external QtIntf name 'QComboBox_hook_create';
procedure QComboBox_hook_destroy(handle: QComboBox_hookH); cdecl; external QtIntf name 'QComboBox_hook_destroy'; 
procedure QComboBox_hook_hook_editTextChanged(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QComboBox_hook_hook_editTextChanged';
procedure QComboBox_hook_hook_activated(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QComboBox_hook_hook_activated';
procedure QComboBox_hook_hook_activated2(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QComboBox_hook_hook_activated2';
procedure QComboBox_hook_hook_highlighted(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QComboBox_hook_hook_highlighted';
procedure QComboBox_hook_hook_highlighted2(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QComboBox_hook_hook_highlighted2';
procedure QComboBox_hook_hook_currentIndexChanged(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QComboBox_hook_hook_currentIndexChanged';
procedure QComboBox_hook_hook_currentIndexChanged2(handle: QComboBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QComboBox_hook_hook_currentIndexChanged2';

function QCheckBox_hook_create(handle: QObjectH): QCheckBox_hookH; cdecl; external QtIntf name 'QCheckBox_hook_create';
procedure QCheckBox_hook_destroy(handle: QCheckBox_hookH); cdecl; external QtIntf name 'QCheckBox_hook_destroy'; 
procedure QCheckBox_hook_hook_stateChanged(handle: QCheckBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QCheckBox_hook_hook_stateChanged';

function QSlider_hook_create(handle: QObjectH): QSlider_hookH; cdecl; external QtIntf name 'QSlider_hook_create';
procedure QSlider_hook_destroy(handle: QSlider_hookH); cdecl; external QtIntf name 'QSlider_hook_destroy'; 

function QTextBrowser_hook_create(handle: QObjectH): QTextBrowser_hookH; cdecl; external QtIntf name 'QTextBrowser_hook_create';
procedure QTextBrowser_hook_destroy(handle: QTextBrowser_hookH); cdecl; external QtIntf name 'QTextBrowser_hook_destroy'; 
procedure QTextBrowser_hook_hook_backwardAvailable(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextBrowser_hook_hook_backwardAvailable';
procedure QTextBrowser_hook_hook_forwardAvailable(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextBrowser_hook_hook_forwardAvailable';
procedure QTextBrowser_hook_hook_sourceChanged(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextBrowser_hook_hook_sourceChanged';
procedure QTextBrowser_hook_hook_highlighted(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextBrowser_hook_hook_highlighted';
procedure QTextBrowser_hook_hook_highlighted2(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextBrowser_hook_hook_highlighted2';
procedure QTextBrowser_hook_hook_anchorClicked(handle: QTextBrowser_hookH; hook: QHookH); cdecl; external QtIntf name 'QTextBrowser_hook_hook_anchorClicked';

function QLabel_hook_create(handle: QObjectH): QLabel_hookH; cdecl; external QtIntf name 'QLabel_hook_create';
procedure QLabel_hook_destroy(handle: QLabel_hookH); cdecl; external QtIntf name 'QLabel_hook_destroy'; 
procedure QLabel_hook_hook_linkActivated(handle: QLabel_hookH; hook: QHookH); cdecl; external QtIntf name 'QLabel_hook_hook_linkActivated';
procedure QLabel_hook_hook_linkHovered(handle: QLabel_hookH; hook: QHookH); cdecl; external QtIntf name 'QLabel_hook_hook_linkHovered';

function QGroupBox_hook_create(handle: QObjectH): QGroupBox_hookH; cdecl; external QtIntf name 'QGroupBox_hook_create';
procedure QGroupBox_hook_destroy(handle: QGroupBox_hookH); cdecl; external QtIntf name 'QGroupBox_hook_destroy'; 
procedure QGroupBox_hook_hook_clicked(handle: QGroupBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QGroupBox_hook_hook_clicked';
procedure QGroupBox_hook_hook_clicked2(handle: QGroupBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QGroupBox_hook_hook_clicked2';
procedure QGroupBox_hook_hook_toggled(handle: QGroupBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QGroupBox_hook_hook_toggled';

function QTabWidget_hook_create(handle: QObjectH): QTabWidget_hookH; cdecl; external QtIntf name 'QTabWidget_hook_create';
procedure QTabWidget_hook_destroy(handle: QTabWidget_hookH); cdecl; external QtIntf name 'QTabWidget_hook_destroy'; 
procedure QTabWidget_hook_hook_currentChanged(handle: QTabWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTabWidget_hook_hook_currentChanged';

function QTabBar_hook_create(handle: QObjectH): QTabBar_hookH; cdecl; external QtIntf name 'QTabBar_hook_create';
procedure QTabBar_hook_destroy(handle: QTabBar_hookH); cdecl; external QtIntf name 'QTabBar_hook_destroy'; 
procedure QTabBar_hook_hook_currentChanged(handle: QTabBar_hookH; hook: QHookH); cdecl; external QtIntf name 'QTabBar_hook_hook_currentChanged';

function QProgressBar_hook_create(handle: QObjectH): QProgressBar_hookH; cdecl; external QtIntf name 'QProgressBar_hook_create';
procedure QProgressBar_hook_destroy(handle: QProgressBar_hookH); cdecl; external QtIntf name 'QProgressBar_hook_destroy'; 
procedure QProgressBar_hook_hook_valueChanged(handle: QProgressBar_hookH; hook: QHookH); cdecl; external QtIntf name 'QProgressBar_hook_hook_valueChanged';

function QStatusBar_hook_create(handle: QObjectH): QStatusBar_hookH; cdecl; external QtIntf name 'QStatusBar_hook_create';
procedure QStatusBar_hook_destroy(handle: QStatusBar_hookH); cdecl; external QtIntf name 'QStatusBar_hook_destroy'; 
procedure QStatusBar_hook_hook_messageChanged(handle: QStatusBar_hookH; hook: QHookH); cdecl; external QtIntf name 'QStatusBar_hook_hook_messageChanged';

function QToolBox_hook_create(handle: QObjectH): QToolBox_hookH; cdecl; external QtIntf name 'QToolBox_hook_create';
procedure QToolBox_hook_destroy(handle: QToolBox_hookH); cdecl; external QtIntf name 'QToolBox_hook_destroy'; 
procedure QToolBox_hook_hook_currentChanged(handle: QToolBox_hookH; hook: QHookH); cdecl; external QtIntf name 'QToolBox_hook_hook_currentChanged';

function QToolButton_hook_create(handle: QObjectH): QToolButton_hookH; cdecl; external QtIntf name 'QToolButton_hook_create';
procedure QToolButton_hook_destroy(handle: QToolButton_hookH); cdecl; external QtIntf name 'QToolButton_hook_destroy'; 
procedure QToolButton_hook_hook_triggered(handle: QToolButton_hookH; hook: QHookH); cdecl; external QtIntf name 'QToolButton_hook_hook_triggered';

function QCalendarWidget_hook_create(handle: QObjectH): QCalendarWidget_hookH; cdecl; external QtIntf name 'QCalendarWidget_hook_create';
procedure QCalendarWidget_hook_destroy(handle: QCalendarWidget_hookH); cdecl; external QtIntf name 'QCalendarWidget_hook_destroy'; 
procedure QCalendarWidget_hook_hook_selectionChanged(handle: QCalendarWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QCalendarWidget_hook_hook_selectionChanged';
procedure QCalendarWidget_hook_hook_clicked(handle: QCalendarWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QCalendarWidget_hook_hook_clicked';
procedure QCalendarWidget_hook_hook_activated(handle: QCalendarWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QCalendarWidget_hook_hook_activated';
procedure QCalendarWidget_hook_hook_currentPageChanged(handle: QCalendarWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QCalendarWidget_hook_hook_currentPageChanged';

function QAbstractItemView_hook_create(handle: QObjectH): QAbstractItemView_hookH; cdecl; external QtIntf name 'QAbstractItemView_hook_create';
procedure QAbstractItemView_hook_destroy(handle: QAbstractItemView_hookH); cdecl; external QtIntf name 'QAbstractItemView_hook_destroy'; 
procedure QAbstractItemView_hook_hook_pressed(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemView_hook_hook_pressed';
procedure QAbstractItemView_hook_hook_clicked(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemView_hook_hook_clicked';
procedure QAbstractItemView_hook_hook_doubleClicked(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemView_hook_hook_doubleClicked';
procedure QAbstractItemView_hook_hook_activated(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemView_hook_hook_activated';
procedure QAbstractItemView_hook_hook_entered(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemView_hook_hook_entered';
procedure QAbstractItemView_hook_hook_viewportEntered(handle: QAbstractItemView_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemView_hook_hook_viewportEntered';

function QListView_hook_create(handle: QObjectH): QListView_hookH; cdecl; external QtIntf name 'QListView_hook_create';
procedure QListView_hook_destroy(handle: QListView_hookH); cdecl; external QtIntf name 'QListView_hook_destroy'; 

function QListWidgetItem_hook_create(handle: QObjectH): QListWidgetItem_hookH; cdecl; external QtIntf name 'QListWidgetItem_hook_create';
procedure QListWidgetItem_hook_destroy(handle: QListWidgetItem_hookH); cdecl; external QtIntf name 'QListWidgetItem_hook_destroy'; 

function QListWidget_hook_create(handle: QObjectH): QListWidget_hookH; cdecl; external QtIntf name 'QListWidget_hook_create';
procedure QListWidget_hook_destroy(handle: QListWidget_hookH); cdecl; external QtIntf name 'QListWidget_hook_destroy'; 
procedure QListWidget_hook_hook_itemPressed(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QListWidget_hook_hook_itemPressed';
procedure QListWidget_hook_hook_itemClicked(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QListWidget_hook_hook_itemClicked';
procedure QListWidget_hook_hook_itemDoubleClicked(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QListWidget_hook_hook_itemDoubleClicked';
procedure QListWidget_hook_hook_itemActivated(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QListWidget_hook_hook_itemActivated';
procedure QListWidget_hook_hook_itemEntered(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QListWidget_hook_hook_itemEntered';
procedure QListWidget_hook_hook_itemChanged(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QListWidget_hook_hook_itemChanged';
procedure QListWidget_hook_hook_currentItemChanged(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QListWidget_hook_hook_currentItemChanged';
procedure QListWidget_hook_hook_currentTextChanged(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QListWidget_hook_hook_currentTextChanged';
procedure QListWidget_hook_hook_currentRowChanged(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QListWidget_hook_hook_currentRowChanged';
procedure QListWidget_hook_hook_itemSelectionChanged(handle: QListWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QListWidget_hook_hook_itemSelectionChanged';

function QTreeView_hook_create(handle: QObjectH): QTreeView_hookH; cdecl; external QtIntf name 'QTreeView_hook_create';
procedure QTreeView_hook_destroy(handle: QTreeView_hookH); cdecl; external QtIntf name 'QTreeView_hook_destroy'; 
procedure QTreeView_hook_hook_expanded(handle: QTreeView_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeView_hook_hook_expanded';
procedure QTreeView_hook_hook_collapsed(handle: QTreeView_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeView_hook_hook_collapsed';

function QTreeWidgetItem_hook_create(handle: QObjectH): QTreeWidgetItem_hookH; cdecl; external QtIntf name 'QTreeWidgetItem_hook_create';
procedure QTreeWidgetItem_hook_destroy(handle: QTreeWidgetItem_hookH); cdecl; external QtIntf name 'QTreeWidgetItem_hook_destroy'; 

function QTreeWidget_hook_create(handle: QObjectH): QTreeWidget_hookH; cdecl; external QtIntf name 'QTreeWidget_hook_create';
procedure QTreeWidget_hook_destroy(handle: QTreeWidget_hookH); cdecl; external QtIntf name 'QTreeWidget_hook_destroy'; 
procedure QTreeWidget_hook_hook_itemPressed(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeWidget_hook_hook_itemPressed';
procedure QTreeWidget_hook_hook_itemClicked(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeWidget_hook_hook_itemClicked';
procedure QTreeWidget_hook_hook_itemDoubleClicked(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeWidget_hook_hook_itemDoubleClicked';
procedure QTreeWidget_hook_hook_itemActivated(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeWidget_hook_hook_itemActivated';
procedure QTreeWidget_hook_hook_itemEntered(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeWidget_hook_hook_itemEntered';
procedure QTreeWidget_hook_hook_itemChanged(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeWidget_hook_hook_itemChanged';
procedure QTreeWidget_hook_hook_itemExpanded(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeWidget_hook_hook_itemExpanded';
procedure QTreeWidget_hook_hook_itemCollapsed(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeWidget_hook_hook_itemCollapsed';
procedure QTreeWidget_hook_hook_currentItemChanged(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeWidget_hook_hook_currentItemChanged';
procedure QTreeWidget_hook_hook_itemSelectionChanged(handle: QTreeWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTreeWidget_hook_hook_itemSelectionChanged';

function QHeaderView_hook_create(handle: QObjectH): QHeaderView_hookH; cdecl; external QtIntf name 'QHeaderView_hook_create';
procedure QHeaderView_hook_destroy(handle: QHeaderView_hookH); cdecl; external QtIntf name 'QHeaderView_hook_destroy'; 
procedure QHeaderView_hook_hook_sectionMoved(handle: QHeaderView_hookH; hook: QHookH); cdecl; external QtIntf name 'QHeaderView_hook_hook_sectionMoved';
procedure QHeaderView_hook_hook_sectionResized(handle: QHeaderView_hookH; hook: QHookH); cdecl; external QtIntf name 'QHeaderView_hook_hook_sectionResized';
procedure QHeaderView_hook_hook_sectionPressed(handle: QHeaderView_hookH; hook: QHookH); cdecl; external QtIntf name 'QHeaderView_hook_hook_sectionPressed';
procedure QHeaderView_hook_hook_sectionClicked(handle: QHeaderView_hookH; hook: QHookH); cdecl; external QtIntf name 'QHeaderView_hook_hook_sectionClicked';
procedure QHeaderView_hook_hook_sectionDoubleClicked(handle: QHeaderView_hookH; hook: QHookH); cdecl; external QtIntf name 'QHeaderView_hook_hook_sectionDoubleClicked';
procedure QHeaderView_hook_hook_sectionCountChanged(handle: QHeaderView_hookH; hook: QHookH); cdecl; external QtIntf name 'QHeaderView_hook_hook_sectionCountChanged';
procedure QHeaderView_hook_hook_sectionHandleDoubleClicked(handle: QHeaderView_hookH; hook: QHookH); cdecl; external QtIntf name 'QHeaderView_hook_hook_sectionHandleDoubleClicked';
procedure QHeaderView_hook_hook_sectionAutoResize(handle: QHeaderView_hookH; hook: QHookH); cdecl; external QtIntf name 'QHeaderView_hook_hook_sectionAutoResize';
procedure QHeaderView_hook_hook_geometriesChanged(handle: QHeaderView_hookH; hook: QHookH); cdecl; external QtIntf name 'QHeaderView_hook_hook_geometriesChanged';

function QStandardItem_hook_create(handle: QObjectH): QStandardItem_hookH; cdecl; external QtIntf name 'QStandardItem_hook_create';
procedure QStandardItem_hook_destroy(handle: QStandardItem_hookH); cdecl; external QtIntf name 'QStandardItem_hook_destroy'; 

function QStandardItemModel_hook_create(handle: QObjectH): QStandardItemModel_hookH; cdecl; external QtIntf name 'QStandardItemModel_hook_create';
procedure QStandardItemModel_hook_destroy(handle: QStandardItemModel_hookH); cdecl; external QtIntf name 'QStandardItemModel_hook_destroy'; 
procedure QStandardItemModel_hook_hook_itemChanged(handle: QStandardItemModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QStandardItemModel_hook_hook_itemChanged';

function QAbstractItemDelegate_hook_create(handle: QObjectH): QAbstractItemDelegate_hookH; cdecl; external QtIntf name 'QAbstractItemDelegate_hook_create';
procedure QAbstractItemDelegate_hook_destroy(handle: QAbstractItemDelegate_hookH); cdecl; external QtIntf name 'QAbstractItemDelegate_hook_destroy'; 
procedure QAbstractItemDelegate_hook_hook_commitData(handle: QAbstractItemDelegate_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemDelegate_hook_hook_commitData';
procedure QAbstractItemDelegate_hook_hook_closeEditor(handle: QAbstractItemDelegate_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemDelegate_hook_hook_closeEditor';
procedure QAbstractItemDelegate_hook_hook_closeEditor2(handle: QAbstractItemDelegate_hookH; hook: QHookH); cdecl; external QtIntf name 'QAbstractItemDelegate_hook_hook_closeEditor2';

function QTableView_hook_create(handle: QObjectH): QTableView_hookH; cdecl; external QtIntf name 'QTableView_hook_create';
procedure QTableView_hook_destroy(handle: QTableView_hookH); cdecl; external QtIntf name 'QTableView_hook_destroy'; 

function QTableWidgetSelectionRange_hook_create(handle: QObjectH): QTableWidgetSelectionRange_hookH; cdecl; external QtIntf name 'QTableWidgetSelectionRange_hook_create';
procedure QTableWidgetSelectionRange_hook_destroy(handle: QTableWidgetSelectionRange_hookH); cdecl; external QtIntf name 'QTableWidgetSelectionRange_hook_destroy'; 

function QTableWidgetItem_hook_create(handle: QObjectH): QTableWidgetItem_hookH; cdecl; external QtIntf name 'QTableWidgetItem_hook_create';
procedure QTableWidgetItem_hook_destroy(handle: QTableWidgetItem_hookH); cdecl; external QtIntf name 'QTableWidgetItem_hook_destroy'; 

function QTableWidget_hook_create(handle: QObjectH): QTableWidget_hookH; cdecl; external QtIntf name 'QTableWidget_hook_create';
procedure QTableWidget_hook_destroy(handle: QTableWidget_hookH); cdecl; external QtIntf name 'QTableWidget_hook_destroy'; 
procedure QTableWidget_hook_hook_itemPressed(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_itemPressed';
procedure QTableWidget_hook_hook_itemClicked(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_itemClicked';
procedure QTableWidget_hook_hook_itemDoubleClicked(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_itemDoubleClicked';
procedure QTableWidget_hook_hook_itemActivated(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_itemActivated';
procedure QTableWidget_hook_hook_itemEntered(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_itemEntered';
procedure QTableWidget_hook_hook_itemChanged(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_itemChanged';
procedure QTableWidget_hook_hook_currentItemChanged(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_currentItemChanged';
procedure QTableWidget_hook_hook_itemSelectionChanged(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_itemSelectionChanged';
procedure QTableWidget_hook_hook_cellPressed(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_cellPressed';
procedure QTableWidget_hook_hook_cellClicked(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_cellClicked';
procedure QTableWidget_hook_hook_cellDoubleClicked(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_cellDoubleClicked';
procedure QTableWidget_hook_hook_cellActivated(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_cellActivated';
procedure QTableWidget_hook_hook_cellEntered(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_cellEntered';
procedure QTableWidget_hook_hook_cellChanged(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_cellChanged';
procedure QTableWidget_hook_hook_currentCellChanged(handle: QTableWidget_hookH; hook: QHookH); cdecl; external QtIntf name 'QTableWidget_hook_hook_currentCellChanged';

function QItemSelectionRange_hook_create(handle: QObjectH): QItemSelectionRange_hookH; cdecl; external QtIntf name 'QItemSelectionRange_hook_create';
procedure QItemSelectionRange_hook_destroy(handle: QItemSelectionRange_hookH); cdecl; external QtIntf name 'QItemSelectionRange_hook_destroy'; 

function QItemSelectionModel_hook_create(handle: QObjectH): QItemSelectionModel_hookH; cdecl; external QtIntf name 'QItemSelectionModel_hook_create';
procedure QItemSelectionModel_hook_destroy(handle: QItemSelectionModel_hookH); cdecl; external QtIntf name 'QItemSelectionModel_hook_destroy'; 
procedure QItemSelectionModel_hook_hook_currentChanged(handle: QItemSelectionModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QItemSelectionModel_hook_hook_currentChanged';
procedure QItemSelectionModel_hook_hook_currentRowChanged(handle: QItemSelectionModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QItemSelectionModel_hook_hook_currentRowChanged';
procedure QItemSelectionModel_hook_hook_currentColumnChanged(handle: QItemSelectionModel_hookH; hook: QHookH); cdecl; external QtIntf name 'QItemSelectionModel_hook_hook_currentColumnChanged';

function QDialog_hook_create(handle: QObjectH): QDialog_hookH; cdecl; external QtIntf name 'QDialog_hook_create';
procedure QDialog_hook_destroy(handle: QDialog_hookH); cdecl; external QtIntf name 'QDialog_hook_destroy'; 
procedure QDialog_hook_hook_finished(handle: QDialog_hookH; hook: QHookH); cdecl; external QtIntf name 'QDialog_hook_hook_finished';
procedure QDialog_hook_hook_accepted(handle: QDialog_hookH; hook: QHookH); cdecl; external QtIntf name 'QDialog_hook_hook_accepted';
procedure QDialog_hook_hook_rejected(handle: QDialog_hookH; hook: QHookH); cdecl; external QtIntf name 'QDialog_hook_hook_rejected';

function QFileDialog_hook_create(handle: QObjectH): QFileDialog_hookH; cdecl; external QtIntf name 'QFileDialog_hook_create';
procedure QFileDialog_hook_destroy(handle: QFileDialog_hookH); cdecl; external QtIntf name 'QFileDialog_hook_destroy'; 
procedure QFileDialog_hook_hook_filesSelected(handle: QFileDialog_hookH; hook: QHookH); cdecl; external QtIntf name 'QFileDialog_hook_hook_filesSelected';
procedure QFileDialog_hook_hook_currentChanged(handle: QFileDialog_hookH; hook: QHookH); cdecl; external QtIntf name 'QFileDialog_hook_hook_currentChanged';

function QProgressDialog_hook_create(handle: QObjectH): QProgressDialog_hookH; cdecl; external QtIntf name 'QProgressDialog_hook_create';
procedure QProgressDialog_hook_destroy(handle: QProgressDialog_hookH); cdecl; external QtIntf name 'QProgressDialog_hook_destroy'; 
procedure QProgressDialog_hook_hook_canceled(handle: QProgressDialog_hookH; hook: QHookH); cdecl; external QtIntf name 'QProgressDialog_hook_hook_canceled';

function QSystemTrayIcon_hook_create(handle: QObjectH): QSystemTrayIcon_hookH; cdecl; external QtIntf name 'QSystemTrayIcon_hook_create';
procedure QSystemTrayIcon_hook_destroy(handle: QSystemTrayIcon_hookH); cdecl; external QtIntf name 'QSystemTrayIcon_hook_destroy'; 
procedure QSystemTrayIcon_hook_hook_activated(handle: QSystemTrayIcon_hookH; hook: QHookH); cdecl; external QtIntf name 'QSystemTrayIcon_hook_hook_activated';
procedure QSystemTrayIcon_hook_hook_messageClicked(handle: QSystemTrayIcon_hookH; hook: QHookH); cdecl; external QtIntf name 'QSystemTrayIcon_hook_hook_messageClicked';

function QGraphicsScene_hook_create(handle: QObjectH): QGraphicsScene_hookH; cdecl; external QtIntf name 'QGraphicsScene_hook_create';
procedure QGraphicsScene_hook_destroy(handle: QGraphicsScene_hookH); cdecl; external QtIntf name 'QGraphicsScene_hook_destroy'; 
procedure QGraphicsScene_hook_hook_sceneRectChanged(handle: QGraphicsScene_hookH; hook: QHookH); cdecl; external QtIntf name 'QGraphicsScene_hook_hook_sceneRectChanged';

function QIODevice_hook_create(handle: QObjectH): QIODevice_hookH; cdecl; external QtIntf name 'QIODevice_hook_create';
procedure QIODevice_hook_destroy(handle: QIODevice_hookH); cdecl; external QtIntf name 'QIODevice_hook_destroy'; 
procedure QIODevice_hook_hook_readyRead(handle: QIODevice_hookH; hook: QHookH); cdecl; external QtIntf name 'QIODevice_hook_hook_readyRead';
procedure QIODevice_hook_hook_bytesWritten(handle: QIODevice_hookH; hook: QHookH); cdecl; external QtIntf name 'QIODevice_hook_hook_bytesWritten';
procedure QIODevice_hook_hook_aboutToClose(handle: QIODevice_hookH; hook: QHookH); cdecl; external QtIntf name 'QIODevice_hook_hook_aboutToClose';



procedure initPWideStrings(CUPS, UOPS, LOPS, IPS, FPS: Pointer); cdecl; external QtIntf name 'initPWideStrings';
procedure InitializePIntArray(GPP, GPL, SPL: Pointer); cdecl; external QtIntf name 'initializePIntArray';


// Special-Purpose Global Functions Exported by Qt

{$IFDEF UNIX}
procedure QtX11WaitForWindowManager(handle : QWidgetH); cdecl; external QtIntf name 'qtx11waitforwindowmanager';
{$ENDIF}

{$IFDEF DARWIN}
procedure QtMacSetDockMenu(handle : QMenuH); cdecl; external QtIntf name 'qtmacsetdockmenu';
{$ENDIF}

implementation
uses SysUtils,Math;

function QtPoint(X,Y:integer): TQtPoint;
begin
  Result.X:=X;
  Result.Y:=Y;
end;


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
procedure CopyUnicodeToPWideString(Unicode: PWideChar; var S: WideString; Len: Integer); cdecl; export;
begin
  SetString(S, Unicode, Len);
end;


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
function GetIntsPtr(PA : PIntArray): PPtrInt; cdecl; export;
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


