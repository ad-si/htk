@REM Batch file that installs the binary files
@REM Obtain the real short form for this directory.
@FOR %%G IN (.) DO @SET WD=%%~sG
@mk\FixFileNames.exe <uni-package.conf.export >uni-package.conf
@mk\FixFileNames.exe %WD% <GHCHTK.CMD.export >GHCHTK.CMD
@mk\FixFileNames.exe %WD% <GHCIHTK.CMD.export >GHCIHTK.CMD
