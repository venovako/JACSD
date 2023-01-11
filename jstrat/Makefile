!IFDEF NDEBUG
DEBUG=
FLAGS=/nologo /std:c17 /DNDEBUG /Ox /EHsc /MD
!ELSE # DEBUG
DEBUG=d
FLAGS=/nologo /std:c17 /Od /EHsc /MDd
!ENDIF

all: ..\jstrat$(DEBUG).lib

..\jstrat$(DEBUG).lib: jstrat.c jstrat.h jstrat_ME.h Makefile
	cl.exe $(FLAGS) jstrat.c /c /Fojstrat$(DEBUG).obj && lib.exe /NOLOGO /OUT:$@ jstrat$(DEBUG).obj

clean:
	-del /F ..\jstrat$(DEBUG).lib jstrat$(DEBUG).obj
