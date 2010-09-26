#!/bin/sh
cd ..
cd parser
DEFAULT_INI="default.ini"
APPKIT_INI="../build/appkit.ini"
FRAMEWORK="/System/Library/Frameworks/AppKit.framework/Headers"
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSApplication.h > ../appkit/NSApplication.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSAttributedString.h > ../appkit/NSAttributedString.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSBitmapImageRep.h > ../appkit/NSBitmapImageRep.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSBox.h > ../appkit/NSBox.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSButton.h > ../appkit/NSButton.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSControl.h > ../appkit/NSControl.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSColor.h > ../appkit/NSColor.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSFont.h > ../appkit/NSFont.inc
./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSFontManager.h > ../appkit/NSFontManager.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSGraphics.h > ../appkit/NSGraphics.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSGraphicsContext.h > ../appkit/NSGraphicsContext.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSImage.h > ../appkit/NSImage.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSImageRep.h > ../appkit/NSImageRep.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSMenu.h > ../appkit/NSMenu.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSMenuItem.h > ../appkit/NSMenuItem.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSNibDeclarations.h > ../appkit/NSNibDeclarations.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSNibLoading.h > ../appkit/NSNibLoading.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSOpenPanel.h > ../appkit/NSOpenPanel.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSPanel.h > ../appkit/NSPanel.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSSavePanel.h > ../appkit/NSSavePanel.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSSpellProtocol.h > ../appkit/NSSpellProtocol.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSStatusBar.h > ../appkit/NSStatusBar.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSStatusItem.h > ../appkit/NSStatusItem.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSText.h > ../appkit/NSText.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSTextField.h > ../appkit/NSTextField.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSTextFieldCell.h > ../appkit/NSTextFieldCell.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSTextView.h > ../appkit/NSTextView.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSToolbar.h > ../appkit/NSToolbar.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSToolbarItem.h > ../appkit/NSToolbarItem.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSUserInterfaceValidation.h > ../appkit/NSUserInterfaceValidation.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSView.h > ../appkit/NSView.inc
#./objcparser -ini=$DEFAULT_INI -ini=$APPKIT_INI $FRAMEWORK/NSWindow.h > ../appkit/NSWindow.inc
