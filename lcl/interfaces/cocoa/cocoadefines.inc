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


