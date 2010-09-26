#!/bin/sh
cd ..
cd parser
DEFAULT_INI="default.ini"
LOCAL_INI="../build/foundation.ini"
FRAMEWORK="/System/Library/Frameworks/Foundation.framework/Headers"
#./objcparser -ini=$DEFAULT_INI -ini=$LOCAL_INI $FRAMEWORK/NSObjCRuntime.h > ../foundation/NSObjCRuntime.inc
#./objcparser -ini=$DEFAULT_INI -ini=$LOCAL_INI $FRAMEWORK/NSArray.h > ../foundation/NSArray.inc
#./objcparser -ini=$DEFAULT_INI -ini=$LOCAL_INI $FRAMEWORK/NSAttributedString.h > ../foundation/NSAttributedString.inc
#./objcparser -ini=$DEFAULT_INI -ini=$LOCAL_INI $FRAMEWORK/NSBundle.h > ../foundation/NSBundle.inc
#./objcparser -ini=$DEFAULT_INI -ini=$LOCAL_INI $FRAMEWORK/NSData.h > ../foundation/NSData.inc
#./objcparser -ini=$DEFAULT_INI -ini=$LOCAL_INI $FRAMEWORK/NSDate.h > ../foundation/NSDate.inc
./objcparser -ini=$DEFAULT_INI -ini=$LOCAL_INI $FRAMEWORK/NSDictionary.h > ../foundation/NSDictionary.inc
#./objcparser -ini=$DEFAULT_INI -ini=$LOCAL_INI $FRAMEWORK/NSRange.h > ../foundation/NSRange.inc
#./objcparser -ini=$DEFAULT_INI -ini=$LOCAL_INI $FRAMEWORK/NSString.h > ../foundation/NSString.inc
#./objcparser -ini=$DEFAULT_INI -ini=$LOCAL_INI $FRAMEWORK/NSValue.h > ../foundation/NSValue.inc
