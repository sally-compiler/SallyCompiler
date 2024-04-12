@echo off

mkdir build >NUL 2>NUL
pushd build

set CFG_OPTIONS=/I"C:\Program Files\Graphviz\include\graphviz" /DYNAMICICBASE "C:\Program Files\Graphviz\lib\*.lib" /D ENABLE_CFG_VIEWER

REM Check if the environmental variable is set?
if DEFINED SALLY_CFG_VIEWER set COMPILE_CFG_VIEWER=%CFG_OPTIONS%

REM Or can be enabled through command line
if /I "%1" == "-cfg" set COMPILE_CFG_VIEWER=%CFG_OPTIONS%

if DEFINED COMPILE_CFG_VIEWER echo Compiling with CFG viewer...

REM cl -nologo -Zi -EHsc ..\code\sally.cpp %COMPILE_CFG_VIEWER%
cl -F10000000 -nologo -Zi -EHsc ..\code\sally.cpp ..\code\arm.cpp ..\code\ast.cpp ..\code\general.cpp  ..\code\parser.cpp ..\code\tokenizer.cpp ..\code\asm\asm_passes.cpp ..\code\opt\opt.cpp ..\code\ir.cpp %COMPILE_CFG_VIEWER%

popd
