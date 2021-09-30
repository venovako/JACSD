all: ..\jstrat.lib

..\jstrat.lib: jstrat.c jstrat.h jstrat_ME.h Makefile
	nvcc.exe -lib -DNDEBUG -o $@ jstrat.c

clean:
	-del /F ..\jstrat.lib
