DEFS=$(wildcard *.def)
TABLES=$(DEFS:.def=)

all: $(TABLES)

clean:
	rm $(TABLES)

%: %.def
	cargo run --bin re_compiler --release --quiet -- $< $@ 2>/dev/null