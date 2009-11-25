{%mainunit appkit.pas}
(*
        NSTextView.h
        Application Kit
        Copyright (c) 1994-2005, Apple Computer, Inc.
        All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSTEXTVIEW_PAS_H}
{$define NSTEXTVIEW_PAS_H}


//#import <AppKit/NSText.h>
//#import <AppKit/NSInputManager.h>
//#import <AppKit/NSTextAttachment.h>
//#import <AppKit/AppKitDefines.h>
//#import <AppKit/NSDragging.h>
//#import <AppKit/NSUserInterfaceValidation.h>
type
  NSSelectionGranularity = (
    NSSelectByCharacter = 0, 
    NSSelectByWord = 1, 
    NSSelectByParagraph = 2
  );


type
  NSSelectionAffinity = (
    NSSelectionAffinityUpstream = 0, 
    NSSelectionAffinityDownstream = 1
  );


type
  NSFindPanelAction = (
    NSFindPanelActionShowFindPanel = 1, 
    NSFindPanelActionNext = 2, 
    NSFindPanelActionPrevious = 3, 
    NSFindPanelActionReplaceAll = 4, 
    NSFindPanelActionReplace = 5, 
    NSFindPanelActionReplaceAndFind = 6, 
    NSFindPanelActionSetFindString = 7, 
    NSFindPanelActionReplaceAllInSelection = 8, 
    NSFindPanelActionSelectAll = 9, 
    NSFindPanelActionSelectAllInSelection = 10
  );


//APPKIT_EXTERN NSString *NSTextViewWillChangeNotifyingTextViewNotification;
//APPKIT_EXTERN NSString *NSTextViewDidChangeSelectionNotification;
//APPKIT_EXTERN NSString *NSTextViewDidChangeTypingAttributesNotification	AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER;
{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextView = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTVIEW_PAS_C}
{$define NSTEXTVIEW_PAS_C}


  { NSTextView }

  NSTextView = class(NSText)
  public
    class function getClass: lobjc.id; override;
    // *************************** Initializing ***************************
    constructor initWithFrame_textContainer(_frameRect: NSRect; _container: lobjc.id {NSTextContainer});
    //  Designated Initializer. container may be nil.
    constructor initWithFrame(_frameRect: NSRect);
    //  This variant will create the text network (textStorage, layoutManager, and a container).
    // **************** Get/Set the container and other stuff ****************
    function textContainer: lobjc.id;{NSTextContainer}
    procedure setTextContainer(_container: lobjc.id {NSTextContainer});
    //  The set method should not be called directly, but you might want to override it.
    //  Gets or sets the text container for this view.  Setting the text container marks
    // the view as needing display.  The text container calls the set method from its setTextView:
    // method.
    procedure replaceTextContainer(_newContainer: lobjc.id {NSTextContainer});
    //  This method should be used instead of the primitive -setTextContainer: if you need
    // to replace a view's text container with a new one leaving the rest of the web intact.
    //  This method deals with all the work of making sure the view doesn't get deallocated
    // and removing the old container from the layoutManager and replacing it with the new
    // one.
    procedure setTextContainerInset(_inset: NSSize);
    function textContainerInset: NSSize;
    //  The textContainerInset determines the padding that the view provides around the
    // container.  The container's origin will be inset by this amount from the bounds point
    // {0,0} and padding will be left to the right and below the container of the same amount.
    //  This inset affects the view sizing in response to new layout and is used by the
    // rectangular text containers when they track the view's frame dimensions.
    function textContainerOrigin: NSPoint;
    procedure invalidateTextContainerOrigin;
    //  The container's origin in the view is determined from the current usage of the container,
    // the container inset, and the view size.  textContainerOrigin returns this point.
    //  invalidateTextContainerOrigin is sent automatically whenever something changes that
    // causes the origin to possibly move.  You usually do not need to call invalidate yourself.
    function layoutManager: lobjc.id;{NSLayoutManager}
    function textStorage: lobjc.id;{NSTextStorage}
    //  Convenience methods
    // ************************ Key binding entry-point ************************
    procedure insertText(_insertString: lobjc.id);
    //  This method is the funnel point for text insertion after keys pass through the key
    // binder.
    // ************************** Sizing methods **************************
    procedure setConstrainedFrameSize(_desiredSize: NSSize);
    //  Sets the frame size of the view to desiredSize constrained within min and max size.
    // **************** New miscellaneous API above and beyond NSText ****************
    procedure setAlignment_range(_alignment: NSTextAlignment; _range: NSRange);
    procedure setBaseWritingDirection_range(_writingDirection: NSWritingDirection; _range: NSRange);
    procedure turnOffKerning(_sender: lobjc.id);
    procedure tightenKerning(_sender: lobjc.id);
    procedure loosenKerning(_sender: lobjc.id);
    procedure useStandardKerning(_sender: lobjc.id);
    procedure turnOffLigatures(_sender: lobjc.id);
    procedure useStandardLigatures(_sender: lobjc.id);
    procedure useAllLigatures(_sender: lobjc.id);
    procedure raiseBaseline(_sender: lobjc.id);
    procedure lowerBaseline(_sender: lobjc.id);
    procedure toggleTraditionalCharacterShape(_sender: lobjc.id);
    procedure outline(_sender: lobjc.id);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure performFindPanelAction(_sender: lobjc.id);
    procedure alignJustified(_sender: lobjc.id);
    procedure changeColor(_sender: lobjc.id);
    procedure changeAttributes(_sender: lobjc.id);
    procedure changeDocumentBackgroundColor(_sender: lobjc.id);
    procedure toggleBaseWritingDirection(_sender: lobjc.id);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    procedure orderFrontSpacingPanel(_sender: lobjc.id);
    procedure orderFrontLinkPanel(_sender: lobjc.id);
    procedure orderFrontListPanel(_sender: lobjc.id);
    procedure orderFrontTablePanel(_sender: lobjc.id);
    procedure rulerView_didMoveMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker});
    procedure rulerView_didRemoveMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker});
    procedure rulerView_didAddMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker});
    function rulerView_shouldMoveMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}): LongBool;
    function rulerView_shouldAddMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}): LongBool;
    function rulerView_willMoveMarker_toLocation(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}; _location: Single): Single;
    function rulerView_shouldRemoveMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}): LongBool;
    function rulerView_willAddMarker_atLocation(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}; _location: Single): Single;
    procedure rulerView_handleMouseDown(_ruler: lobjc.id {NSRulerView}; _event: lobjc.id {NSEvent});
    // ************************** Fine display control **************************
    procedure setNeedsDisplayInRect_avoidAdditionalLayout(_rect: NSRect; _flag: LongBool);
    function shouldDrawInsertionPoint: LongBool;
    procedure drawInsertionPointInRect_color_turnedOn(_rect: NSRect; _color: lobjc.id {NSColor}; _flag: LongBool);
    procedure drawViewBackgroundInRect(_rect: NSRect);
    procedure updateRuler;
    procedure updateFontPanel;
    procedure updateDragTypeRegistration;
    function selectionRangeForProposedRange_granularity(_proposedCharRange: NSRange; _granularity: NSSelectionGranularity): NSRange;
    // ************************** Especially for subclassers **************************
    procedure clickedOnLink_atIndex(_link: lobjc.id; _charIndex: LongWord);
    //  Cause the text view to act as if someone clicked on a piece of text with link as
    // the value of NSLinkAttributeName.  If, for instance, you have a special attachment
    // cell that can end up following links, you can use this method to ask the text view
    // to follow a link once you decide it should.  This method is invoked by the text view
    // during mouse tracking if the user is clicking a link as well.  This sends the textView:clickedOnLink:
    // delegation if the delegate responds.
    // ************************ Speech support ************************
    procedure startSpeaking(_sender: lobjc.id);
    procedure stopSpeaking(_sender: lobjc.id);
    // ************************ Completion support ********************
    procedure complete(_sender: lobjc.id);
    //  Responder method for invoking completion.  May be invoked programmatically if autocompletion
    // is desired.
    function rangeForUserCompletion: NSRange;
    //  Usually returns the partial range from the most recent beginning of a word up to
    // the insertion point.  May be overridden by subclassers to alter the range to be completed.
    //  Returning (NSNotFound, 0) suppresses completion.
    function completionsForPartialWordRange_indexOfSelectedItem(_charRange: NSRange; __index: Integer): CFArrayRef;
    //  Returns an array of potential completions, in the order to be presented, representing
    // complete words that the user might be trying to type when starting by typing the
    // partial word at the given range.  May be overridden by subclassers to modify or override
    // this list.  Returning nil or a zero-length array suppresses completion.   The selected
    // item index may optionally be set to indicate which completion should be initially
    // selected; default is 0, and -1 indicates no selection.  This method should call the
    // delegate method textView:completions:forPartialWordRange:indexOfSelectedItem: if
    // implemented.
    procedure insertCompletion_forPartialWordRange_movement_isFinal(_word: CFStringRef; _charRange: NSRange; _movement: Integer; _flag: LongBool);
    //  Called with final == NO as the user moves through the potential completions, then
    // with final == YES when a completion is definitively selected (or completion is cancelled
    // and the original value is reinserted).  The default implementation inserts the completion
    // into the text at the appropriate location.  The movement argument takes its values
    // from the movement codes defined in NSText.h, and allows subclassers to distinguish
    // between cancelling completion and selection by arrow keys, by return, by tab, or
    // by other means such as clicking.
    // ****************** Pasteboard support (mainly for subclassers) ******************
    //  To implement support for writing a new type, override -writeSelectionToPasteboard:type:
    // to support your new type (and call super if you're not going to handle it.)  Then
    // you can override -writablePasteboardTypes and optionally the +registerForServices
    // methods to enable the new type.
    //  To implement support for reading a new type, override -readSelectionFromPasteboard:type:
    // to support your new type (and call super if you're not going to handle it.)  Then
    // you can override -readablePasteboardTypes and optionally the +registerForServices
    // and acceptableDragTypes methods to enable the new type.  Advanced drag acceptance
    // behavior can be implemented with dragOperationForDraggingInfo:type: and cleanUpAfterDragOperation.
    //  To implement new mechanisms that cause data to be written to the pasteboard you
    // can call writablePasteboardTypes and writeSelectionToPasteboard:types:.
    //  To implement new mechanisms that cause data to be read from the pasteboard you can
    // call preferredPasteboardTypeFromArray:restrictedToTypesFromArray: and readSelectionFromPasteboard:type:.
    function writablePasteboardTypes: CFArrayRef;
    //  Returns an array of pasteboard types that can be provided from the current selection.
    //  Overriders should copy the result from super and add their own new types.
    function writeSelectionToPasteboard_type(_pboard: lobjc.id {NSPasteboard}; __type: CFStringRef): LongBool;
    //  Invoked automatically to write a single type to the pasteboard.  The type will already
    // have been declared to the pasteboard so this should merely write the data using the
    // appropriate set method on the pasteboard.
    function writeSelectionToPasteboard_types(_pboard: lobjc.id {NSPasteboard}; _types: CFArrayRef): LongBool;
    //  Delclares all the types to the pasteboard then calls writeSelectionToPasteboard:type:
    // for each type in the array.
    function readablePasteboardTypes: CFArrayRef;
    //  Returns an array of types that could be read currently in order of preference. 
    // Subclassers should take care to consider the "preferred" part of the semantics of
    // this method.  Figure out where your new type should fit into the preferred order.
    //  The preferred order should usually be from the richest types down to the less rich.
    //  The default array will start with RTFD, RTF, strings, files, images, colors and
    // so on.  The ordering list really starts to lose significance after the first few
    // elements.  If the new format you are supporting is richer than RTFD, put it at the
    // head of the list, otherwise try to find the right place for it, but don't count on
    // the actual contents of the list you get from super either.
    function preferredPasteboardTypeFromArray_restrictedToTypesFromArray(_availableTypes: CFArrayRef; _allowedTypes: CFArrayRef): CFStringRef;
    //  Returns the most preferred type from the available types array that it is currently
    // possible to read.  If allowedTypes is provided then only those types will be considered
    // regardless of whether others could currently be read.  You should not have to override
    // this to support new types.
    function readSelectionFromPasteboard_type(_pboard: lobjc.id {NSPasteboard}; __type: CFStringRef): LongBool;
    //  Invoked automatically to read a specific type from the pasteboard.  The type will
    // already have been by the preferredPasteboardTypeFromArray:restrictedToTypesFromArray:
    // method so this should merely read the data using the appropriate accessor method
    // on the pasteboard.
    function readSelectionFromPasteboard(_pboard: lobjc.id {NSPasteboard}): LongBool;
    //  Part of the services mechanism.  This is implemented such that you should not need
    // to override it to support new types.  It should not be necessary to call this.
    class procedure registerForServices;
    //  Automatically invoked when the first instance of NSTextView is created.  Subclassing
    // this is necessary to add support for new service types (in addition to actually supporting
    // writing or reading the types, of course).  Override it to call super and then register
    // your own new types.  You should probably never need to call this except to message
    // super in an override.
    function validRequestorForSendType_returnType(_sendType: CFStringRef; _returnType: CFStringRef): lobjc.id;
    //  This method is part of the services protocol and is implemented in such a way that
    // if you extend the type support correctly you should not need to override it.  You
    // should never need to call it either.
    procedure pasteAsPlainText(_sender: lobjc.id);
    procedure pasteAsRichText(_sender: lobjc.id);
    //  These methods are like paste: (from NSResponder) but they restrict the acceptable
    // type of the pasted data.  They are suitable as menu actions for appropriate "Paste
    // As" submenu commands.
    function dragSelectionWithEvent_offset_slideBack(_event: lobjc.id {NSEvent}; _mouseOffset: NSSize; _slideBack: LongBool): LongBool;
    //  Causes textview to begin dragging current selected range, returning YES if it succeeds
    // in initiating the drag.  Primarily for subclassers, who can override it to intervene
    // at beginning of a drag.
    function dragImageForSelectionWithEvent_origin(_event: lobjc.id {NSEvent}; _origin: NSPointPointer): lobjc.id;{NSImage}
    //  Used by dragSelectionWithEvent:offset:slideBack: to get an appropriate image.  Returns
    // the lower-left point of the image in view coordinates as origin.  Can be called by
    // others who need such an image, or can be overridden by subclassers to return a different
    // image (if it returns nil, a default icon will be used).
    function acceptableDragTypes: CFArrayRef;
    //  Must be overridden to support new drag types in addition to adding support for reading
    // and writing the type.  Override it to call super, then make a copy of the result
    // and add your own new types before returning the copy.  You should probably never
    // need to call this except to message super in an override.
    function dragOperationForDraggingInfo_type(_dragInfo: lobjc.id; __type: CFStringRef): LongWord;
    //  This is called by draggingEntered:... and draggingUpdated:...  It should return
    // the drag operation constant appropriate to the current situation or NSDragOperationNone.
    //  It can also do any other auxilliary stuff like drawing a ghost to indicate where
    // the dragged thing will go when it is dropped.  Be aware that this is called over
    // and over so if you're going to draw a ghost or something avoid doing it over and
    // over unless you need to.  Any state you set up in this method that needs to be cleared
    // can be cleared by overriding the next method.  You should probably never need to
    // call this except to message super in an override.
    procedure cleanUpAfterDragOperation;
    function acceptsGlyphInfo: LongBool;
    procedure setAcceptsGlyphInfo(_flag: LongBool);
    function selectedRanges: CFArrayRef;
    procedure setSelectedRanges_affinity_stillSelecting(_ranges: CFArrayRef; _affinity: NSSelectionAffinity; _stillSelectingFlag: LongBool);
    procedure setSelectedRanges(_ranges: CFArrayRef);
    procedure setSelectedRange_affinity_stillSelecting(_charRange: NSRange; _affinity: NSSelectionAffinity; _stillSelectingFlag: LongBool);
    function selectionAffinity: NSSelectionAffinity;
    function selectionGranularity: NSSelectionGranularity;
    procedure setSelectionGranularity(_granularity: NSSelectionGranularity);
    procedure setSelectedTextAttributes(_attributeDictionary: CFDictionaryRef);
    function selectedTextAttributes: CFDictionaryRef;
    procedure setInsertionPointColor(_color: lobjc.id {NSColor});
    function insertionPointColor: lobjc.id;{NSColor}
    procedure updateInsertionPointStateAndRestartTimer(_restartFlag: LongBool);
    procedure setMarkedTextAttributes(_attributeDictionary: CFDictionaryRef);
    function markedTextAttributes: CFDictionaryRef;
    procedure setLinkTextAttributes(_attributeDictionary: CFDictionaryRef);
    function linkTextAttributes: CFDictionaryRef;
    procedure setRulerVisible(_flag: LongBool);
    function usesRuler: LongBool;
    procedure setUsesRuler(_flag: LongBool);
    procedure setContinuousSpellCheckingEnabled(_flag: LongBool);
    function isContinuousSpellCheckingEnabled: LongBool;
    procedure toggleContinuousSpellChecking(_sender: lobjc.id);
    function spellCheckerDocumentTag: Integer;
    function typingAttributes: CFDictionaryRef;
    procedure setTypingAttributes(_attrs: CFDictionaryRef);
    function shouldChangeTextInRanges_replacementStrings(_affectedRanges: CFArrayRef; _replacementStrings: CFArrayRef): LongBool;
    function rangesForUserTextChange: CFArrayRef;
    function rangesForUserCharacterAttributeChange: CFArrayRef;
    function rangesForUserParagraphAttributeChange: CFArrayRef;
    function shouldChangeTextInRange_replacementString(_affectedCharRange: NSRange; _replacementString: CFStringRef): LongBool;
    procedure didChangeText;
    function rangeForUserTextChange: NSRange;
    function rangeForUserCharacterAttributeChange: NSRange;
    function rangeForUserParagraphAttributeChange: NSRange;
    procedure setUsesFindPanel(_flag: LongBool);
    function usesFindPanel: LongBool;
    procedure setAllowsDocumentBackgroundColorChange(_flag: LongBool);
    function allowsDocumentBackgroundColorChange: LongBool;
    procedure setDefaultParagraphStyle(_paragraphStyle: lobjc.id {NSParagraphStyle});
    function defaultParagraphStyle: lobjc.id;{NSParagraphStyle}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    procedure breakUndoCoalescing;
    function isSelectable: LongBool;
    procedure setSelectable(_flag: LongBool);
    function isEditable: LongBool;
    procedure setEditable(_flag: LongBool);
    function isRichText: LongBool;
    procedure setRichText(_flag: LongBool);
    function importsGraphics: LongBool;
    procedure setImportsGraphics(_flag: LongBool);
    function delegate: lobjc.id;
    procedure setDelegate(_anObject: lobjc.id);
    function isFieldEditor: LongBool;
    procedure setFieldEditor(_flag: LongBool);
    function usesFontPanel: LongBool;
    procedure setUsesFontPanel(_flag: LongBool);
    function isRulerVisible: LongBool;
    procedure setBackgroundColor(_color: lobjc.id {NSColor});
    function backgroundColor: lobjc.id;{NSColor}
    procedure setDrawsBackground(_flag: LongBool);
    function drawsBackground: LongBool;
    procedure setSelectedRange(_charRange: NSRange);
    function allowsUndo: LongBool;
    procedure setAllowsUndo(_flag: LongBool);
    // ************************** NSResponder methods **************************
    function resignFirstResponder: LongBool;
    function becomeFirstResponder: LongBool;
    // ************************** Smart copy/paste/delete support **************************
    function smartInsertDeleteEnabled: LongBool;
    procedure setSmartInsertDeleteEnabled(_flag: LongBool);
    function smartDeleteRangeForProposedRange(_proposedCharRange: NSRange): NSRange;
    procedure smartInsertForString_replacingRange_beforeString_afterString(_pasteString: CFStringRef; _charRangeToReplace: NSRange; _beforeString: CFStringRef; _afterString: CFStringRef);
    function smartInsertBeforeStringForString_replacingRange(_pasteString: CFStringRef; _charRangeToReplace: NSRange): CFStringRef;
    function smartInsertAfterStringForString_replacingRange(_pasteString: CFStringRef; _charRangeToReplace: NSRange): CFStringRef;
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSTextView_NSTextView = 'NSTextView';
  StrNSTextView_initWithFrame_textContainer = 'initWithFrame:textContainer:';
  StrNSTextView_initWithFrame = 'initWithFrame:';
  StrNSTextView_textContainer = 'textContainer';
  StrNSTextView_setTextContainer = 'setTextContainer:';
  StrNSTextView_replaceTextContainer = 'replaceTextContainer:';
  StrNSTextView_setTextContainerInset = 'setTextContainerInset:';
  StrNSTextView_textContainerInset = 'textContainerInset';
  StrNSTextView_textContainerOrigin = 'textContainerOrigin';
  StrNSTextView_invalidateTextContainerOrigin = 'invalidateTextContainerOrigin';
  StrNSTextView_layoutManager = 'layoutManager';
  StrNSTextView_textStorage = 'textStorage';
  StrNSTextView_insertText = 'insertText:';
  StrNSTextView_setConstrainedFrameSize = 'setConstrainedFrameSize:';
  StrNSTextView_setAlignment_range = 'setAlignment:range:';
  StrNSTextView_setBaseWritingDirection_range = 'setBaseWritingDirection:range:';
  StrNSTextView_turnOffKerning = 'turnOffKerning:';
  StrNSTextView_tightenKerning = 'tightenKerning:';
  StrNSTextView_loosenKerning = 'loosenKerning:';
  StrNSTextView_useStandardKerning = 'useStandardKerning:';
  StrNSTextView_turnOffLigatures = 'turnOffLigatures:';
  StrNSTextView_useStandardLigatures = 'useStandardLigatures:';
  StrNSTextView_useAllLigatures = 'useAllLigatures:';
  StrNSTextView_raiseBaseline = 'raiseBaseline:';
  StrNSTextView_lowerBaseline = 'lowerBaseline:';
  StrNSTextView_toggleTraditionalCharacterShape = 'toggleTraditionalCharacterShape:';
  StrNSTextView_outline = 'outline:';
  StrNSTextView_performFindPanelAction = 'performFindPanelAction:';
  StrNSTextView_alignJustified = 'alignJustified:';
  StrNSTextView_changeColor = 'changeColor:';
  StrNSTextView_changeAttributes = 'changeAttributes:';
  StrNSTextView_changeDocumentBackgroundColor = 'changeDocumentBackgroundColor:';
  StrNSTextView_toggleBaseWritingDirection = 'toggleBaseWritingDirection:';
  StrNSTextView_orderFrontSpacingPanel = 'orderFrontSpacingPanel:';
  StrNSTextView_orderFrontLinkPanel = 'orderFrontLinkPanel:';
  StrNSTextView_orderFrontListPanel = 'orderFrontListPanel:';
  StrNSTextView_orderFrontTablePanel = 'orderFrontTablePanel:';
  StrNSTextView_rulerView_didMoveMarker = 'rulerView:didMoveMarker:';
  StrNSTextView_rulerView_didRemoveMarker = 'rulerView:didRemoveMarker:';
  StrNSTextView_rulerView_didAddMarker = 'rulerView:didAddMarker:';
  StrNSTextView_rulerView_shouldMoveMarker = 'rulerView:shouldMoveMarker:';
  StrNSTextView_rulerView_shouldAddMarker = 'rulerView:shouldAddMarker:';
  StrNSTextView_rulerView_willMoveMarker_toLocation = 'rulerView:willMoveMarker:toLocation:';
  StrNSTextView_rulerView_shouldRemoveMarker = 'rulerView:shouldRemoveMarker:';
  StrNSTextView_rulerView_willAddMarker_atLocation = 'rulerView:willAddMarker:atLocation:';
  StrNSTextView_rulerView_handleMouseDown = 'rulerView:handleMouseDown:';
  StrNSTextView_setNeedsDisplayInRect_avoidAdditionalLayout = 'setNeedsDisplayInRect:avoidAdditionalLayout:';
  StrNSTextView_shouldDrawInsertionPoint = 'shouldDrawInsertionPoint';
  StrNSTextView_drawInsertionPointInRect_color_turnedOn = 'drawInsertionPointInRect:color:turnedOn:';
  StrNSTextView_drawViewBackgroundInRect = 'drawViewBackgroundInRect:';
  StrNSTextView_updateRuler = 'updateRuler';
  StrNSTextView_updateFontPanel = 'updateFontPanel';
  StrNSTextView_updateDragTypeRegistration = 'updateDragTypeRegistration';
  StrNSTextView_selectionRangeForProposedRange_granularity = 'selectionRangeForProposedRange:granularity:';
  StrNSTextView_clickedOnLink_atIndex = 'clickedOnLink:atIndex:';
  StrNSTextView_startSpeaking = 'startSpeaking:';
  StrNSTextView_stopSpeaking = 'stopSpeaking:';

  StrNSTextView_complete = 'complete:';
  StrNSTextView_rangeForUserCompletion = 'rangeForUserCompletion';
  StrNSTextView_completionsForPartialWordRange_indexOfSelectedItem = 'completionsForPartialWordRange:indexOfSelectedItem:';
  StrNSTextView_insertCompletion_forPartialWordRange_movement_isFinal = 'insertCompletion:forPartialWordRange:movement:isFinal:';

  StrNSTextView_writablePasteboardTypes = 'writablePasteboardTypes';
  StrNSTextView_writeSelectionToPasteboard_type = 'writeSelectionToPasteboard:type:';
  StrNSTextView_writeSelectionToPasteboard_types = 'writeSelectionToPasteboard:types:';
  StrNSTextView_readablePasteboardTypes = 'readablePasteboardTypes';
  StrNSTextView_preferredPasteboardTypeFromArray_restrictedToTypesFromArray = 'preferredPasteboardTypeFromArray:restrictedToTypesFromArray:';
  StrNSTextView_readSelectionFromPasteboard_type = 'readSelectionFromPasteboard:type:';
  StrNSTextView_readSelectionFromPasteboard = 'readSelectionFromPasteboard:';
  StrNSTextView_registerForServices = 'registerForServices';
  StrNSTextView_validRequestorForSendType_returnType = 'validRequestorForSendType:returnType:';
  StrNSTextView_pasteAsPlainText = 'pasteAsPlainText:';
  StrNSTextView_pasteAsRichText = 'pasteAsRichText:';

  StrNSTextView_dragSelectionWithEvent_offset_slideBack = 'dragSelectionWithEvent:offset:slideBack:';
  StrNSTextView_dragImageForSelectionWithEvent_origin = 'dragImageForSelectionWithEvent:origin:';
  StrNSTextView_acceptableDragTypes = 'acceptableDragTypes';
  StrNSTextView_dragOperationForDraggingInfo_type = 'dragOperationForDraggingInfo:type:';
  StrNSTextView_cleanUpAfterDragOperation = 'cleanUpAfterDragOperation';
  StrNSTextView_acceptsGlyphInfo = 'acceptsGlyphInfo';
  StrNSTextView_setAcceptsGlyphInfo = 'setAcceptsGlyphInfo:';

  StrNSTextView_selectedRanges = 'selectedRanges';
  StrNSTextView_setSelectedRanges_affinity_stillSelecting = 'setSelectedRanges:affinity:stillSelecting:';
  StrNSTextView_setSelectedRanges = 'setSelectedRanges:';
  StrNSTextView_setSelectedRange_affinity_stillSelecting = 'setSelectedRange:affinity:stillSelecting:';
  StrNSTextView_selectionAffinity = 'selectionAffinity';
  StrNSTextView_selectionGranularity = 'selectionGranularity';
  StrNSTextView_setSelectionGranularity = 'setSelectionGranularity:';
  StrNSTextView_setSelectedTextAttributes = 'setSelectedTextAttributes:';
  StrNSTextView_selectedTextAttributes = 'selectedTextAttributes';
  StrNSTextView_setInsertionPointColor = 'setInsertionPointColor:';
  StrNSTextView_insertionPointColor = 'insertionPointColor';
  StrNSTextView_updateInsertionPointStateAndRestartTimer = 'updateInsertionPointStateAndRestartTimer:';
  StrNSTextView_setMarkedTextAttributes = 'setMarkedTextAttributes:';
  StrNSTextView_markedTextAttributes = 'markedTextAttributes';
  StrNSTextView_setLinkTextAttributes = 'setLinkTextAttributes:';
  StrNSTextView_linkTextAttributes = 'linkTextAttributes';
  StrNSTextView_setRulerVisible = 'setRulerVisible:';
  StrNSTextView_usesRuler = 'usesRuler';
  StrNSTextView_setUsesRuler = 'setUsesRuler:';
  StrNSTextView_setContinuousSpellCheckingEnabled = 'setContinuousSpellCheckingEnabled:';
  StrNSTextView_isContinuousSpellCheckingEnabled = 'isContinuousSpellCheckingEnabled';
  StrNSTextView_toggleContinuousSpellChecking = 'toggleContinuousSpellChecking:';
  StrNSTextView_spellCheckerDocumentTag = 'spellCheckerDocumentTag';
  StrNSTextView_typingAttributes = 'typingAttributes';
  StrNSTextView_setTypingAttributes = 'setTypingAttributes:';
  StrNSTextView_shouldChangeTextInRanges_replacementStrings = 'shouldChangeTextInRanges:replacementStrings:';
  StrNSTextView_rangesForUserTextChange = 'rangesForUserTextChange';
  StrNSTextView_rangesForUserCharacterAttributeChange = 'rangesForUserCharacterAttributeChange';
  StrNSTextView_rangesForUserParagraphAttributeChange = 'rangesForUserParagraphAttributeChange';
  StrNSTextView_shouldChangeTextInRange_replacementString = 'shouldChangeTextInRange:replacementString:';
  StrNSTextView_didChangeText = 'didChangeText';
  StrNSTextView_rangeForUserTextChange = 'rangeForUserTextChange';
  StrNSTextView_rangeForUserCharacterAttributeChange = 'rangeForUserCharacterAttributeChange';
  StrNSTextView_rangeForUserParagraphAttributeChange = 'rangeForUserParagraphAttributeChange';
  StrNSTextView_setUsesFindPanel = 'setUsesFindPanel:';
  StrNSTextView_usesFindPanel = 'usesFindPanel';
  StrNSTextView_setAllowsDocumentBackgroundColorChange = 'setAllowsDocumentBackgroundColorChange:';
  StrNSTextView_allowsDocumentBackgroundColorChange = 'allowsDocumentBackgroundColorChange';
  StrNSTextView_setDefaultParagraphStyle = 'setDefaultParagraphStyle:';
  StrNSTextView_defaultParagraphStyle = 'defaultParagraphStyle';
  StrNSTextView_breakUndoCoalescing = 'breakUndoCoalescing';
  StrNSTextView_isSelectable = 'isSelectable';
  StrNSTextView_setSelectable = 'setSelectable:';
  StrNSTextView_isEditable = 'isEditable';
  StrNSTextView_setEditable = 'setEditable:';
  StrNSTextView_isRichText = 'isRichText';
  StrNSTextView_setRichText = 'setRichText:';
  StrNSTextView_importsGraphics = 'importsGraphics';
  StrNSTextView_setImportsGraphics = 'setImportsGraphics:';
  StrNSTextView_delegate = 'delegate';
  StrNSTextView_setDelegate = 'setDelegate:';
  StrNSTextView_isFieldEditor = 'isFieldEditor';
  StrNSTextView_setFieldEditor = 'setFieldEditor:';
  StrNSTextView_usesFontPanel = 'usesFontPanel';
  StrNSTextView_setUsesFontPanel = 'setUsesFontPanel:';
  StrNSTextView_isRulerVisible = 'isRulerVisible';
  StrNSTextView_setBackgroundColor = 'setBackgroundColor:';
  StrNSTextView_backgroundColor = 'backgroundColor';
  StrNSTextView_setDrawsBackground = 'setDrawsBackground:';
  StrNSTextView_drawsBackground = 'drawsBackground';
  StrNSTextView_setSelectedRange = 'setSelectedRange:';
  StrNSTextView_allowsUndo = 'allowsUndo';
  StrNSTextView_setAllowsUndo = 'setAllowsUndo:';
  StrNSTextView_resignFirstResponder = 'resignFirstResponder';
  StrNSTextView_becomeFirstResponder = 'becomeFirstResponder';
  StrNSTextView_smartInsertDeleteEnabled = 'smartInsertDeleteEnabled';
  StrNSTextView_setSmartInsertDeleteEnabled = 'setSmartInsertDeleteEnabled:';
  StrNSTextView_smartDeleteRangeForProposedRange = 'smartDeleteRangeForProposedRange:';
  StrNSTextView_smartInsertForString_replacingRange_beforeString_afterString = 'smartInsertForString:replacingRange:beforeString:afterString:';
  StrNSTextView_smartInsertBeforeStringForString_replacingRange = 'smartInsertBeforeStringForString:replacingRange:';
  StrNSTextView_smartInsertAfterStringForString_replacingRange = 'smartInsertAfterStringForString:replacingRange:';

{ NSTextView }

class function NSTextView.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSTextView_NSTextView);
end;

constructor NSTextView.initWithFrame_textContainer(_frameRect: NSRect; _container: lobjc.id {NSTextContainer});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_frameRect: NSRect; _container: lobjc.id {NSTextContainer}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSTextView_initWithFrame_textContainer)), _frameRect, _container);
end;

constructor NSTextView.initWithFrame(_frameRect: NSRect);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_frameRect: NSRect): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSTextView_initWithFrame)), _frameRect);
end;

function NSTextView.textContainer: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_textContainer)), []));
end;

procedure NSTextView.setTextContainer(_container: lobjc.id {NSTextContainer});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_container: lobjc.id {NSTextContainer}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setTextContainer)), _container);
end;

procedure NSTextView.replaceTextContainer(_newContainer: lobjc.id {NSTextContainer});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newContainer: lobjc.id {NSTextContainer}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_replaceTextContainer)), _newContainer);
end;

procedure NSTextView.setTextContainerInset(_inset: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_inset: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setTextContainerInset)), _inset);
end;

function NSTextView.textContainerInset: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSTextView_textContainerInset)), []);
end;

function NSTextView.textContainerOrigin: NSPoint;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSTextView_textContainerOrigin)), []);
end;

procedure NSTextView.invalidateTextContainerOrigin;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_invalidateTextContainerOrigin)), []);
end;

function NSTextView.layoutManager: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_layoutManager)), []));
end;

function NSTextView.textStorage: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_textStorage)), []));
end;

procedure NSTextView.insertText(_insertString: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_insertString: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_insertText)), _insertString);
end;

procedure NSTextView.setConstrainedFrameSize(_desiredSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_desiredSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setConstrainedFrameSize)), _desiredSize);
end;

procedure NSTextView.setAlignment_range(_alignment: NSTextAlignment; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_alignment: NSTextAlignment; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setAlignment_range)), _alignment, _range);
end;

procedure NSTextView.setBaseWritingDirection_range(_writingDirection: NSWritingDirection; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_writingDirection: NSWritingDirection; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setBaseWritingDirection_range)), _writingDirection, _range);
end;

procedure NSTextView.turnOffKerning(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_turnOffKerning)), _sender);
end;

procedure NSTextView.tightenKerning(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_tightenKerning)), _sender);
end;

procedure NSTextView.loosenKerning(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_loosenKerning)), _sender);
end;

procedure NSTextView.useStandardKerning(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_useStandardKerning)), _sender);
end;

procedure NSTextView.turnOffLigatures(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_turnOffLigatures)), _sender);
end;

procedure NSTextView.useStandardLigatures(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_useStandardLigatures)), _sender);
end;

procedure NSTextView.useAllLigatures(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_useAllLigatures)), _sender);
end;

procedure NSTextView.raiseBaseline(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_raiseBaseline)), _sender);
end;

procedure NSTextView.lowerBaseline(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_lowerBaseline)), _sender);
end;

procedure NSTextView.toggleTraditionalCharacterShape(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_toggleTraditionalCharacterShape)), _sender);
end;

procedure NSTextView.outline(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_outline)), _sender);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSTextView.performFindPanelAction(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_performFindPanelAction)), _sender);
end;

procedure NSTextView.alignJustified(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_alignJustified)), _sender);
end;

procedure NSTextView.changeColor(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_changeColor)), _sender);
end;

procedure NSTextView.changeAttributes(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_changeAttributes)), _sender);
end;

procedure NSTextView.changeDocumentBackgroundColor(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_changeDocumentBackgroundColor)), _sender);
end;

procedure NSTextView.toggleBaseWritingDirection(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_toggleBaseWritingDirection)), _sender);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
procedure NSTextView.orderFrontSpacingPanel(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_orderFrontSpacingPanel)), _sender);
end;

procedure NSTextView.orderFrontLinkPanel(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_orderFrontLinkPanel)), _sender);
end;

procedure NSTextView.orderFrontListPanel(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_orderFrontListPanel)), _sender);
end;

procedure NSTextView.orderFrontTablePanel(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_orderFrontTablePanel)), _sender);
end;

procedure NSTextView.rulerView_didMoveMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_rulerView_didMoveMarker)), _ruler, _marker);
end;

procedure NSTextView.rulerView_didRemoveMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_rulerView_didRemoveMarker)), _ruler, _marker);
end;

procedure NSTextView.rulerView_didAddMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_rulerView_didAddMarker)), _ruler, _marker);
end;

function NSTextView.rulerView_shouldMoveMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextView_rulerView_shouldMoveMarker)), _ruler, _marker));
end;

function NSTextView.rulerView_shouldAddMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextView_rulerView_shouldAddMarker)), _ruler, _marker));
end;

function NSTextView.rulerView_willMoveMarker_toLocation(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}; _location: Single): Single;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}; _location: Single): Single; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Single(vmethod(Handle, sel_registerName(PChar(StrNSTextView_rulerView_willMoveMarker_toLocation)), _ruler, _marker, _location));
end;

function NSTextView.rulerView_shouldRemoveMarker(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextView_rulerView_shouldRemoveMarker)), _ruler, _marker));
end;

function NSTextView.rulerView_willAddMarker_atLocation(_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}; _location: Single): Single;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_ruler: lobjc.id {NSRulerView}; _marker: lobjc.id {NSRulerMarker}; _location: Single): Single; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Single(vmethod(Handle, sel_registerName(PChar(StrNSTextView_rulerView_willAddMarker_atLocation)), _ruler, _marker, _location));
end;

procedure NSTextView.rulerView_handleMouseDown(_ruler: lobjc.id {NSRulerView}; _event: lobjc.id {NSEvent});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_ruler: lobjc.id {NSRulerView}; _event: lobjc.id {NSEvent}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_rulerView_handleMouseDown)), _ruler, _event);
end;

procedure NSTextView.setNeedsDisplayInRect_avoidAdditionalLayout(_rect: NSRect; _flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect; _flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setNeedsDisplayInRect_avoidAdditionalLayout)), _rect, _flag);
end;

function NSTextView.shouldDrawInsertionPoint: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_shouldDrawInsertionPoint)), []));
end;

procedure NSTextView.drawInsertionPointInRect_color_turnedOn(_rect: NSRect; _color: lobjc.id {NSColor}; _flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect; _color: lobjc.id {NSColor}; _flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_drawInsertionPointInRect_color_turnedOn)), _rect, _color, _flag);
end;

procedure NSTextView.drawViewBackgroundInRect(_rect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_drawViewBackgroundInRect)), _rect);
end;

procedure NSTextView.updateRuler;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_updateRuler)), []);
end;

procedure NSTextView.updateFontPanel;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_updateFontPanel)), []);
end;

procedure NSTextView.updateDragTypeRegistration;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_updateDragTypeRegistration)), []);
end;

function NSTextView.selectionRangeForProposedRange_granularity(_proposedCharRange: NSRange; _granularity: NSSelectionGranularity): NSRange;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_proposedCharRange: NSRange; _granularity: NSSelectionGranularity); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSTextView_selectionRangeForProposedRange_granularity)), _proposedCharRange, _granularity);
end;

procedure NSTextView.clickedOnLink_atIndex(_link: lobjc.id; _charIndex: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_link: lobjc.id; _charIndex: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_clickedOnLink_atIndex)), _link, _charIndex);
end;

procedure NSTextView.startSpeaking(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_startSpeaking)), _sender);
end;

procedure NSTextView.stopSpeaking(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_stopSpeaking)), _sender);
end;

procedure NSTextView.complete(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_complete)), _sender);
end;

function NSTextView.rangeForUserCompletion: NSRange;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSTextView_rangeForUserCompletion)), []);
end;

function NSTextView.completionsForPartialWordRange_indexOfSelectedItem(_charRange: NSRange; __index: Integer): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_charRange: NSRange; __index: Integer): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(Handle, sel_registerName(PChar(StrNSTextView_completionsForPartialWordRange_indexOfSelectedItem)), _charRange, __index));
end;

procedure NSTextView.insertCompletion_forPartialWordRange_movement_isFinal(_word: CFStringRef; _charRange: NSRange; _movement: Integer; _flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_word: CFStringRef; _charRange: NSRange; _movement: Integer; _flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_insertCompletion_forPartialWordRange_movement_isFinal)), _word, _charRange, _movement, _flag);
end;

function NSTextView.writablePasteboardTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_writablePasteboardTypes)), []));
end;

function NSTextView.writeSelectionToPasteboard_type(_pboard: lobjc.id {NSPasteboard}; __type: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pboard: lobjc.id {NSPasteboard}; __type: CFStringRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextView_writeSelectionToPasteboard_type)), _pboard, __type));
end;

function NSTextView.writeSelectionToPasteboard_types(_pboard: lobjc.id {NSPasteboard}; _types: CFArrayRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pboard: lobjc.id {NSPasteboard}; _types: CFArrayRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextView_writeSelectionToPasteboard_types)), _pboard, _types));
end;

function NSTextView.readablePasteboardTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_readablePasteboardTypes)), []));
end;

function NSTextView.preferredPasteboardTypeFromArray_restrictedToTypesFromArray(_availableTypes: CFArrayRef; _allowedTypes: CFArrayRef): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_availableTypes: CFArrayRef; _allowedTypes: CFArrayRef): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSTextView_preferredPasteboardTypeFromArray_restrictedToTypesFromArray)), _availableTypes, _allowedTypes));
end;

function NSTextView.readSelectionFromPasteboard_type(_pboard: lobjc.id {NSPasteboard}; __type: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pboard: lobjc.id {NSPasteboard}; __type: CFStringRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextView_readSelectionFromPasteboard_type)), _pboard, __type));
end;

function NSTextView.readSelectionFromPasteboard(_pboard: lobjc.id {NSPasteboard}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pboard: lobjc.id {NSPasteboard}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextView_readSelectionFromPasteboard)), _pboard));
end;

class procedure NSTextView.registerForServices;
begin
  objc_msgSend(getClass, sel_registerName(PChar(StrNSTextView_registerForServices)), []);
end;

function NSTextView.validRequestorForSendType_returnType(_sendType: CFStringRef; _returnType: CFStringRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_sendType: CFStringRef; _returnType: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSTextView_validRequestorForSendType_returnType)), _sendType, _returnType));
end;

procedure NSTextView.pasteAsPlainText(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_pasteAsPlainText)), _sender);
end;

procedure NSTextView.pasteAsRichText(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_pasteAsRichText)), _sender);
end;

function NSTextView.dragSelectionWithEvent_offset_slideBack(_event: lobjc.id {NSEvent}; _mouseOffset: NSSize; _slideBack: LongBool): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_event: lobjc.id {NSEvent}; _mouseOffset: NSSize; _slideBack: LongBool): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextView_dragSelectionWithEvent_offset_slideBack)), _event, _mouseOffset, _slideBack));
end;

function NSTextView.dragImageForSelectionWithEvent_origin(_event: lobjc.id {NSEvent}; _origin: NSPointPointer): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_event: lobjc.id {NSEvent}; _origin: NSPointPointer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSTextView_dragImageForSelectionWithEvent_origin)), _event, _origin));
end;

function NSTextView.acceptableDragTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_acceptableDragTypes)), []));
end;

function NSTextView.dragOperationForDraggingInfo_type(_dragInfo: lobjc.id; __type: CFStringRef): LongWord;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_dragInfo: lobjc.id; __type: CFStringRef): LongWord; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongWord(vmethod(Handle, sel_registerName(PChar(StrNSTextView_dragOperationForDraggingInfo_type)), _dragInfo, __type));
end;

procedure NSTextView.cleanUpAfterDragOperation;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_cleanUpAfterDragOperation)), []);
end;

function NSTextView.acceptsGlyphInfo: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_acceptsGlyphInfo)), []));
end;

procedure NSTextView.setAcceptsGlyphInfo(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setAcceptsGlyphInfo)), _flag);
end;

function NSTextView.selectedRanges: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_selectedRanges)), []));
end;

procedure NSTextView.setSelectedRanges_affinity_stillSelecting(_ranges: CFArrayRef; _affinity: NSSelectionAffinity; _stillSelectingFlag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_ranges: CFArrayRef; _affinity: NSSelectionAffinity; _stillSelectingFlag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setSelectedRanges_affinity_stillSelecting)), _ranges, _affinity, _stillSelectingFlag);
end;

procedure NSTextView.setSelectedRanges(_ranges: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_ranges: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setSelectedRanges)), _ranges);
end;

procedure NSTextView.setSelectedRange_affinity_stillSelecting(_charRange: NSRange; _affinity: NSSelectionAffinity; _stillSelectingFlag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_charRange: NSRange; _affinity: NSSelectionAffinity; _stillSelectingFlag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setSelectedRange_affinity_stillSelecting)), _charRange, _affinity, _stillSelectingFlag);
end;

function NSTextView.selectionAffinity: NSSelectionAffinity;
begin
  Result := NSSelectionAffinity(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_selectionAffinity)), []));
end;

function NSTextView.selectionGranularity: NSSelectionGranularity;
begin
  Result := NSSelectionGranularity(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_selectionGranularity)), []));
end;

procedure NSTextView.setSelectionGranularity(_granularity: NSSelectionGranularity);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_granularity: NSSelectionGranularity); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setSelectionGranularity)), _granularity);
end;

procedure NSTextView.setSelectedTextAttributes(_attributeDictionary: CFDictionaryRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_attributeDictionary: CFDictionaryRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setSelectedTextAttributes)), _attributeDictionary);
end;

function NSTextView.selectedTextAttributes: CFDictionaryRef;
begin
  Result := CFDictionaryRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_selectedTextAttributes)), []));
end;

procedure NSTextView.setInsertionPointColor(_color: lobjc.id {NSColor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_color: lobjc.id {NSColor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setInsertionPointColor)), _color);
end;

function NSTextView.insertionPointColor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_insertionPointColor)), []));
end;

procedure NSTextView.updateInsertionPointStateAndRestartTimer(_restartFlag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_restartFlag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_updateInsertionPointStateAndRestartTimer)), _restartFlag);
end;

procedure NSTextView.setMarkedTextAttributes(_attributeDictionary: CFDictionaryRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_attributeDictionary: CFDictionaryRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setMarkedTextAttributes)), _attributeDictionary);
end;

function NSTextView.markedTextAttributes: CFDictionaryRef;
begin
  Result := CFDictionaryRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_markedTextAttributes)), []));
end;

procedure NSTextView.setLinkTextAttributes(_attributeDictionary: CFDictionaryRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_attributeDictionary: CFDictionaryRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setLinkTextAttributes)), _attributeDictionary);
end;

function NSTextView.linkTextAttributes: CFDictionaryRef;
begin
  Result := CFDictionaryRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_linkTextAttributes)), []));
end;

procedure NSTextView.setRulerVisible(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setRulerVisible)), _flag);
end;

function NSTextView.usesRuler: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_usesRuler)), []));
end;

procedure NSTextView.setUsesRuler(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setUsesRuler)), _flag);
end;

procedure NSTextView.setContinuousSpellCheckingEnabled(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setContinuousSpellCheckingEnabled)), _flag);
end;

function NSTextView.isContinuousSpellCheckingEnabled: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_isContinuousSpellCheckingEnabled)), []));
end;

procedure NSTextView.toggleContinuousSpellChecking(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_toggleContinuousSpellChecking)), _sender);
end;

function NSTextView.spellCheckerDocumentTag: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_spellCheckerDocumentTag)), []));
end;

function NSTextView.typingAttributes: CFDictionaryRef;
begin
  Result := CFDictionaryRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_typingAttributes)), []));
end;

procedure NSTextView.setTypingAttributes(_attrs: CFDictionaryRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_attrs: CFDictionaryRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setTypingAttributes)), _attrs);
end;

function NSTextView.shouldChangeTextInRanges_replacementStrings(_affectedRanges: CFArrayRef; _replacementStrings: CFArrayRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_affectedRanges: CFArrayRef; _replacementStrings: CFArrayRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextView_shouldChangeTextInRanges_replacementStrings)), _affectedRanges, _replacementStrings));
end;

function NSTextView.rangesForUserTextChange: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_rangesForUserTextChange)), []));
end;

function NSTextView.rangesForUserCharacterAttributeChange: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_rangesForUserCharacterAttributeChange)), []));
end;

function NSTextView.rangesForUserParagraphAttributeChange: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_rangesForUserParagraphAttributeChange)), []));
end;

function NSTextView.shouldChangeTextInRange_replacementString(_affectedCharRange: NSRange; _replacementString: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_affectedCharRange: NSRange; _replacementString: CFStringRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextView_shouldChangeTextInRange_replacementString)), _affectedCharRange, _replacementString));
end;

procedure NSTextView.didChangeText;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_didChangeText)), []);
end;

function NSTextView.rangeForUserTextChange: NSRange;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSTextView_rangeForUserTextChange)), []);
end;

function NSTextView.rangeForUserCharacterAttributeChange: NSRange;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSTextView_rangeForUserCharacterAttributeChange)), []);
end;

function NSTextView.rangeForUserParagraphAttributeChange: NSRange;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSTextView_rangeForUserParagraphAttributeChange)), []);
end;

procedure NSTextView.setUsesFindPanel(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setUsesFindPanel)), _flag);
end;

function NSTextView.usesFindPanel: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_usesFindPanel)), []));
end;

procedure NSTextView.setAllowsDocumentBackgroundColorChange(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setAllowsDocumentBackgroundColorChange)), _flag);
end;

function NSTextView.allowsDocumentBackgroundColorChange: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_allowsDocumentBackgroundColorChange)), []));
end;

procedure NSTextView.setDefaultParagraphStyle(_paragraphStyle: lobjc.id {NSParagraphStyle});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_paragraphStyle: lobjc.id {NSParagraphStyle}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setDefaultParagraphStyle)), _paragraphStyle);
end;

function NSTextView.defaultParagraphStyle: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_defaultParagraphStyle)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
procedure NSTextView.breakUndoCoalescing;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_breakUndoCoalescing)), []);
end;

function NSTextView.isSelectable: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_isSelectable)), []));
end;

procedure NSTextView.setSelectable(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setSelectable)), _flag);
end;

function NSTextView.isEditable: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_isEditable)), []));
end;

procedure NSTextView.setEditable(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setEditable)), _flag);
end;

function NSTextView.isRichText: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_isRichText)), []));
end;

procedure NSTextView.setRichText(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setRichText)), _flag);
end;

function NSTextView.importsGraphics: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_importsGraphics)), []));
end;

procedure NSTextView.setImportsGraphics(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setImportsGraphics)), _flag);
end;

function NSTextView.delegate: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_delegate)), []));
end;

procedure NSTextView.setDelegate(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setDelegate)), _anObject);
end;

function NSTextView.isFieldEditor: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_isFieldEditor)), []));
end;

procedure NSTextView.setFieldEditor(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setFieldEditor)), _flag);
end;

function NSTextView.usesFontPanel: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_usesFontPanel)), []));
end;

procedure NSTextView.setUsesFontPanel(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setUsesFontPanel)), _flag);
end;

function NSTextView.isRulerVisible: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_isRulerVisible)), []));
end;

procedure NSTextView.setBackgroundColor(_color: lobjc.id {NSColor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_color: lobjc.id {NSColor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setBackgroundColor)), _color);
end;

function NSTextView.backgroundColor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_backgroundColor)), []));
end;

procedure NSTextView.setDrawsBackground(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setDrawsBackground)), _flag);
end;

function NSTextView.drawsBackground: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_drawsBackground)), []));
end;

procedure NSTextView.setSelectedRange(_charRange: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_charRange: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setSelectedRange)), _charRange);
end;

function NSTextView.allowsUndo: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_allowsUndo)), []));
end;

procedure NSTextView.setAllowsUndo(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setAllowsUndo)), _flag);
end;

function NSTextView.resignFirstResponder: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_resignFirstResponder)), []));
end;

function NSTextView.becomeFirstResponder: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_becomeFirstResponder)), []));
end;

function NSTextView.smartInsertDeleteEnabled: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextView_smartInsertDeleteEnabled)), []));
end;

procedure NSTextView.setSmartInsertDeleteEnabled(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_setSmartInsertDeleteEnabled)), _flag);
end;

function NSTextView.smartDeleteRangeForProposedRange(_proposedCharRange: NSRange): NSRange;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_proposedCharRange: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSTextView_smartDeleteRangeForProposedRange)), _proposedCharRange);
end;

procedure NSTextView.smartInsertForString_replacingRange_beforeString_afterString(_pasteString: CFStringRef; _charRangeToReplace: NSRange; _beforeString: CFStringRef; _afterString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_pasteString: CFStringRef; _charRangeToReplace: NSRange; _beforeString: CFStringRef; _afterString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextView_smartInsertForString_replacingRange_beforeString_afterString)), _pasteString, _charRangeToReplace, _beforeString, _afterString);
end;

function NSTextView.smartInsertBeforeStringForString_replacingRange(_pasteString: CFStringRef; _charRangeToReplace: NSRange): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pasteString: CFStringRef; _charRangeToReplace: NSRange): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSTextView_smartInsertBeforeStringForString_replacingRange)), _pasteString, _charRangeToReplace));
end;

function NSTextView.smartInsertAfterStringForString_replacingRange(_pasteString: CFStringRef; _charRangeToReplace: NSRange): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pasteString: CFStringRef; _charRangeToReplace: NSRange): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(Handle, sel_registerName(PChar(StrNSTextView_smartInsertAfterStringForString_replacingRange)), _pasteString, _charRangeToReplace));
end;

{$endif}
