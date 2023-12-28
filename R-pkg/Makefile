all: clean pkg

.PHONY: all clean install

pkg: rpm/src/*.c rpm/src/*.h rpm/DESCRIPTION rpm/NAMESPACE rpm/R/* rpm/tests/*
	R CMD build rpm
	R CMD check rpm --no-manual

install:
	R CMD INSTALL rpm

test:
	R CMD check rpm --no-manual

clean:
	rm -f rpm/src/*.o rpm/src/*.a rpm/src/*.so rpm/src/symbols.rds rpm/src/*.dll
