REM Adjust these paths to yours
SET PATH=C:\Programas\android-sdk\tools;C:\Programas\android-sdk\platform-tools\;C:\Progra~1\Java\jdk1.6.0_20\bin

adb uninstall com.pascal.lcltest
adb install bin\androidlcltest.apk

pause
