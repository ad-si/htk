@REM Batch file that installs the binary files
@mk\FixFileNames.exe <uni-package.conf.export >uni-package.conf
@mk\FixFileNames.exe %WD% <GHCHTK.BAT.export >GHCHTK.BAT
@mk\FixFileNames.exe %WD% <GHCIHTK.BAT.export >GHCIHTK.BAT
