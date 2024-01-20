.PHONY: all clean tests

all: ppurs.exe

ppurs.exe: 
	dune build 
	mv ./src/ppurs.exe ./ppurs.exe

clean:
	dune clean
	rm -f ppurs.exe
	rm -f test/exec/*.s
	rm -f test/out

tests: ppurs.exe
	cd test && ./test.sh -1 ../ppurs.exe
	cd test && ./test.sh -2 ../ppurs.exe
	cd test && ./test.sh -3 ../ppurs.exe
