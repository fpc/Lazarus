REM Adjust these paths to yours
SET PATH=C:\Programas\android-sdk\tools;C:\Programas\android-sdk\platform-tools\;C:\Progra~1\Java\jdk1.6.0_20\bin
SET ANDROID_HOME=C:\Programas\android-sdk
SET APK_SDK_PLATFORM=C:\Programas\android-sdk\platforms\android-8
SET APK_PROJECT_PATH=F:\Programas\lazarussvn\examples\androidlcl\android

mkdir bin
mkdir bin\classes
mkdir gen
mkdir gen\com
mkdir gen\com\pascal
mkdir gen\com\pascal\lcltest

REM Resource compilation
aapt p -f -M AndroidManifest.xml -F bin\androidlcltest.ap_ -I %APK_SDK_PLATFORM%\android.jar -S res -m -J gen

REM Java compiler
javac -classpath %APK_SDK_PLATFORM%\android.jar -d bin\classes src\com\pascal\lcltest\LCLActivity.java

REM DX to convert the java bytecode to dalvik bytecode
dx --dex --no-strict --output=%APK_PROJECT_PATH%\bin\classes.dex %APK_PROJECT_PATH%\bin\classes\com\pascal\lcltest\LCLActivity.class

REM Now build the unsigned APK
apkbuilder bin\androidlcltest-unsigned.apk -u -z bin\androidlcltest.ap_ -f bin\classes.dex

pause
