
all: plugin
	gnat make -p -Pmain

plugin:
	gnat make -p -Pcomputer

clean:
	gnat clean -r -Pmain
	rm -fr plugins lib obj
	rm -f *~
