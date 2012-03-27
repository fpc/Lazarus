REM Adjust these paths to yours
SET PATH=C:\Programas\android-sdk\tools;C:\Programas\android-sdk\platform-tools\;C:\Progra~1\Java\jdk1.6.0_20\bin
SET APP_NAME=androidlcltest
SET ANDROID_HOME=C:\Programas\android-sdk
SET APK_SDK_PLATFORM=C:\Programas\android-sdk\platforms\android-8
SET APK_PROJECT_PATH=F:\Programas\lazarussvn\examples\androidlcl\android

mkdir bin
mkdir bin\classes
mkdir gen
mkdir gen\com
mkdir gen\com\pascal
mkdir gen\com\pascal\lcltest

REM Cleanup
del bin\%APP_NAME%.ap_

REM Resource compilation
call aapt p -f -M AndroidManifest.xml -F bin\%APP_NAME%.ap_ -I %APK_SDK_PLATFORM%\android.jar -S res -m -J gen
aapt a bin\%APP_NAME%.ap_ libs\armeabi\liblclapp.so

REM Java compiler
call javac -classpath %APK_SDK_PLATFORM%\android.jar -d bin\classes src\com\pascal\lcltest\LCLActivity.java

REM DX to convert the java bytecode to dalvik bytecode
call dx --dex --output=%APK_PROJECT_PATH%\bin\classes.dex %APK_PROJECT_PATH%\bin\classes

REM It seams that dx calls echo off
@echo on
REM Now build the unsigned APK
call apkbuilder %APK_PROJECT_PATH%\bin\%APP_NAME%-unsigned.apk -u -z %APK_PROJECT_PATH%\bin\%APP_NAME%.ap_ -f %APK_PROJECT_PATH%\bin\classes.dex

REM Generating on the fly a debug key
REM keytool -genkey -v -keystore bin\LCLDebugKey.keystore -alias LCLDebugKey -keyalg RSA -validity 10000 -dname NAME -keypass senhas

REM Signing the APK with a debug key
jarsigner -keystore bin\LCLDebugKey.keystore -keypass senhas -signedjar bin\%APP_NAME%.apk bin\%APP_NAME%-unsigned.apk LCLDebugKey

REM call and pause together allow us to see the results in the end
pause
