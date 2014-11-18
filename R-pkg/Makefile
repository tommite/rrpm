all: pkg

.PHONY: all clean install

LD_LIBRARY_PATH=/usr/lib/lp_solve

pkg: rpm/src/*.c rpm/src/*.h rpm/DESCRIPTION rpm/NAMESPACE rpm/R/* rpm/tests/*
	rm -f rpm/src/*.o rpm/src/*.so
	R CMD build rpm
	R CMD check rpm

install:
	R CMD INSTALL rpm

test:
	R CMD check rpm

clean:
	rm -f rpm/src/*.o rpm/src/*.so rpm/src/symbols.rds
