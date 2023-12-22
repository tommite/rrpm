all: pkg

.PHONY: all clean install

pkg: rpm/src/*.c rpm/src/*.h rpm/DESCRIPTION rpm/NAMESPACE rpm/R/* rpm/tests/*
	rm -f rpm/src/*.o rpm/src/*.a rpm/src/*.so rpm/src/*.dll
	R CMD build rpm
	R CMD check rpm

install:
	R CMD INSTALL rpm

test:
	R CMD check rpm

clean:
	rm -f rpm/src/*.o rpm/src/*.so rpm/src/symbols.rds
