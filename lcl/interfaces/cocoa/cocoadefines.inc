{.$define COCOA_USE_NATIVE_MODAL}

// There's an issue identified with passing boolean parameters.
// with FPC 3.0.4. see: https://bugs.freepascal.org/view.php?id=34411
//
// In short: Boolean is being passed only as 8-bits value, leaving
// other registers untouched. Apple code (compiler) however,
// reads the entire 32-bit value of the register.
// x86_64 ABI is not entirely complete regarding the proper ways
//
// The issue is presumably only for 64-bit platform.
// The workaround is possible! the issue should be fixed in future
// release of FPC, but 3.0.4 is the offical supported by LCL.
{$define BOOLFIX}


// Originally LCL-Cocoa would override "run" method and have direct control
// over the event loop. However that presumed to cause issues in macOS 10.15
// The code was changed not to override "run" loop, but instead override
// the first request to process an event, and run LCL loop from there.
// Such approach is some what an ugly solution, yet it's reliable, in a sense
// that Cocoa performs ALL of this methods.
{$define COCOALOOPOVERRIDE}

// Not override "run" method. Catch any FPC exception
// The biggest problem of the Native approach - LCL "runloop" method is not called
// at all. Thus if LCL implementation is changed, CocoaWS needs to be updated
{.$define COCOALOOPNATIVE}

{$if not defined(COCOALOOPOVERRIDE) and not defined(COCOALOOPNATIVE)}
// the first call to nextEventMatchingMask_untilDate_inMode_dequeue would
// cause an LCL event processing loop to be called.
// the call stays there until, LCL application is terminated
{$define COCOALOOPHIJACK}
{$endif}

{$ifdef COCOALOOPOVERRIDE}
  // The proper switching modal dialogs depends on NSApp.isRunning method
  // The method returns true (on macOS 10.15 and later), only with "run" method
  // called direclty.
  // (there's an internal flag, that's defines the value of the property)
  // Since LCL needs the direct control over the event loop (with Loopoverride)
  // it canot call the inherited NSapp.run. (or the control is lossed to Cocoa)
  //
  // Instead, CocoaWS can set isRunning property to true, in TWO ways
  //   1) (COCOAPPRUNNING_OVERRIDEPROPERTY)
  //      override isRunning method and return TRUE when wanted
  //   2) (COCOAPPRUNNING_SETINTPROPERTY)
  //      using KeyValue APIs set the internal property
  // Either method is not entirely good, but it makes modal switching work.
  //
  // The actual problem (of not having isRunning set to true), is the following:
  // 1) Create a window.
  // 2) Show a modal dialog.
  // 3) Close the modal window
  // 4) the focus stays in the closed and hidden modal window...
  // with NSApp.isRunning, the focus goes back to the previously active window

  {$define COCOAPPRUNNING_OVERRIDEPROPERTY}
  {.$define COCOAPPRUNNING_SETINTPROPERTY}
{$endif}

// COCOA_NATIVEACTIVATION allows to prevent COCOA_ACTIVATION_REORDER
{$ifndef COCOA_NATIVEACTIVATION}
// The order of the windows needs to be restored, when the application
// is reactivated. This is due to the LCL modality used.
//
// However, resorting windows is impacting 10.15 File Dialogs.
// File Dialogs are duplicated with an external application.
// The File Dialog is loosing its accessory view (file type selection).
{$define COCOA_ACTIVATION_REORDER}
{$endif}
