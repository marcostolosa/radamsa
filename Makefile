DESTDIR=
PREFIX=/usr
BINDIR=/bin
CFLAGS?=-Wall -O3
LDFLAGS?=
OFLAGS=-O1
OWLURL=https://haltp.org/files/ol-0.2.2.c.gz
USR_BIN_OL?=/usr/bin/ol

everything: bin/radamsa

build_radamsa:
	test -x $(USR_BIN_OL)
	$(USR_BIN_OL) $(OFLAGS) -o radamsa.c rad/main.scm
	mkdir -p bin
	$(CC) $(CFLAGS) $(LDFLAGS) -o bin/radamsa radamsa.c

bin/radamsa: radamsa.c
	mkdir -p bin
	$(CC) $(CFLAGS) $(LDFLAGS) -o bin/radamsa radamsa.c

radamsa.c: rad/*.scm lib/hex
	test -x bin/ol || make bin/ol
	bin/ol $(OFLAGS) -o radamsa.c rad/main.scm

lib/hex:
	mkdir -p lib
	cd lib && git clone https://gitlab.com/owl-lisp/hex.git

radamsa.fasl: rad/*.scm bin/ol
	bin/ol -o radamsa.fasl rad/main.scm

ol.c:
	test -f ol.c.gz || wget -O ol.c.gz $(OWLURL) || curl -L -o ol.c.gz $(OWLURL)
	gzip -d < ol.c.gz > ol.c

bin/ol: ol.c
	mkdir -p bin
	cc $(CFLAGS) -o bin/ol ol.c

install: bin/radamsa
	-mkdir -p $(DESTDIR)$(PREFIX)/bin
	cp bin/radamsa $(DESTDIR)$(PREFIX)/bin
	-mkdir -p $(DESTDIR)$(PREFIX)/share/man/man1
	cat doc/radamsa.1 | gzip -9 > $(DESTDIR)$(PREFIX)/share/man/man1/radamsa.1.gz

clean:
	-rm -f radamsa.c c/libradamsa.c lib/libradamsa.a lib/libradamsa.so bin/radamsa .seal-of-quality
	-rm -f bin/ol

mrproper: clean
	-rm -rf ol.*
	-rm -rf lib/hex

test: .seal-of-quality

fasltest: radamsa.fasl
	sh tests/run owl-lisp/bin/vm radamsa.fasl

.seal-of-quality: bin/radamsa
	-mkdir -p tmp
	sh tests/run bin/radamsa
	touch .seal-of-quality

# a quick to compile vanilla bytecode executable
bytecode: bin/ol
	bin/ol -O0 -x c -o - rad/main.scm | $(CC) $(CFLAGS) -x c -o bin/radamsa -
	-mkdir -p tmp
	sh tests/run bin/radamsa

# a simple mutation benchmark
benchmark: bin/radamsa
	tests/benchmark bin/radamsa

future:
	test -d owl || git clone https://gitlab.com/owl-lisp/owl
	#cd owl && git pull
	-cd owl && make bin/ol
	cp owl/bin/ol bin/ol
	make

autofuzz: bin/radamsa
	mkdir -p tmp
	echo '<html> <foo bar=baz>zeb</foo> <foo babar=lol></html>' > tmp/test.xmlish
	cp radamsa.c tmp/test.c
	cp /bin/sh tmp/test.bin
	echo "HAL 9000" > tmp/test.small
	# create sample data in 3 rounds
	bin/radamsa -o tmp/test.%n -n 100 rad/* bin/* tmp/test.*
	bin/radamsa -o tmp/test.2.%n -n 100 rad/* bin/* tmp/test.*
	bin/radamsa -o tmp/test.3.%n -n 100 rad/* bin/* tmp/test.*
	# fuzz 100k
	bin/radamsa -v --meta autofuzz.log -n 100000 tmp/test.* > /dev/null
	echo autofuzz success


## Library mode test

c/libradamsa.c: bin/ol c/lib.c rad/*.scm
	bin/ol $(OFLAGS) --mode library -o c/libradamsa.c rad/libradamsa.scm
	sed -i 's/int main/int secondary/' c/libradamsa.c
	cat c/lib.c >> c/libradamsa.c

lib/libradamsa.o: c/libradamsa.c
	mkdir -p lib
	cc $(CFLAGS) -I c -o lib/libradamsa.o -c c/libradamsa.c

lib/libradamsa.a: lib/libradamsa.o
	ar crs lib/libradamsa.a lib/libradamsa.o

lib/libradamsa.so: c/libradamsa.c
	mkdir -p lib
	# temporary hack
	sed -i -e '/radamsa\.h/d' c/libradamsa.c
	cc -shared $(CFLAGS) c/libradamsa.c -o lib/libradamsa.so -fPIC

bin/libradamsa-test: lib/libradamsa.so c/libradamsa-test.c
	mkdir -p tmp
	cc $(CFLAGS) -Ic -o bin/libradamsa-test c/libradamsa-test.c -Llib -lradamsa

libradamsa-test: bin/libradamsa-test
	LD_LIBRARY_PATH=lib:$(LD_LIBRARY_PATH) DYLD_LIBRARY_PATH=lib:$(DYLD_LIBRARY_PATH) bin/libradamsa-test c/lib.c | grep "library test passed"


## Cleanup and Meta

uninstall:
	rm $(DESTDIR)$(PREFIX)/bin/radamsa || echo "no radamsa"
	rm $(DESTDIR)$(PREFIX)/share/man/man1/radamsa.1.gz || echo "no manpage"

.PHONY: todo you install clean mrproper test bytecode uninstall get-owl standalone
