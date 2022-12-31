!IFDEF NDEBUG
DEBUG=
FLAGS=-DNDEBUG
!ELSE # DEBUG
DEBUG=d
FLAGS=-g
!ENDIF

all: ..\jstrat$(DEBUG).lib

..\jstrat$(DEBUG).lib: jstrat.c jstrat.h jstrat_ME.h Makefile
	nvcc.exe $(FLAGS) -lib -o $@ jstrat.c

clean:
	-del /F ..\jstrat$(DEBUG).lib
