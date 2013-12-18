IDRIS := idris

build: .PHONY
	$(IDRIS) --build parallax.ipkg

install: 
	$(IDRIS) --install parallax.ipkg

clean: .PHONY
	$(IDRIS) --clean parallax.ipkg

rebuild: clean build

linecount: .PHONY
	find . -name '*.idr' | xargs wc -l

.PHONY:
