all: pkg

.PHONY: all clean install

LD_LIBRARY_PATH=/usr/lib/lp_solve

pkg: rpm/src/*.c rpm/src/*.h rpm/DESCRIPTION rpm/NAMESPACE
	rm -f rpm/src/*.o rpm/src/*.so
	R CMD build rpm
	R CMD check rpm

install:
	R CMD install rpm-*tar.gz

clean:
	rm -rf rpm.Rcheck
	rm -f rpm/src/*.o rpm/src/*.so rpm/src/symbols.rds
