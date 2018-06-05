cd %CD%\..\..\six\src

set EXPORT_INC=..\export\include

for /f "delims=" %%i in ('dir /b /a-d /s "*.h"')  do copy /Y %%i %EXPORT_INC%

for /f "delims=" %%i in ('dir /b /a-d /s "*.hpp"')  do copy /Y %%i %EXPORT_INC%

pause