REM Adjust these paths to yours
SET PATH=C:\Programas\android-sdk\tools;C:\Programas\android-sdk\platform-tools\;C:\Progra~1\Java\jdk1.6.0_37\bin
SET APP_NAME=androidlcltest
SET ANDROID_HOME=C:\Programas\android-sdk
SET APK_SDK_PLATFORM=C:\Programas\android-sdk\platforms\android-8
SET APK_PROJECT_PATH=F:\Programas\lazarussvn\examples\androidlcl\android

mkdir bin

keytool --help

keytool -genkey -v -keystore bin\LCLDebugKey.keystore -alias LCLDebugKey -keyalg RSA -validity 10000

REM call and pause together allow us to see the results in the end
pause
