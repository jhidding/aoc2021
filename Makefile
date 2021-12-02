.PHONY: all clean

pandoc_args += -s -t markdown-fenced_divs+yaml_metadata_block
pandoc_args += --lua-filter scripts/gnuplot.lua
pandoc_args += --lua-filter scripts/annotate.lua
# pandoc_args += --template template/template.html
pandoc_input := $(wildcard lit/day*.md) lit/boilerplate.md
pandoc_output := $(pandoc_input:%.md=_site/content/%.md)

all: docs

clean:
	rm -rf docs
	rm -rf _site/content/*

docs: $(pandoc_output) _site/content/index.md
	cd _site && hugo
	mv _site/public docs

# _site:
#	hugo new site _site

_site/content/index.md: README.md _site
	cp $< $@

_site/content/lit/%.md: lit/%.md _site
	@mkdir -p $(@D)
	pandoc -t markdown $(pandoc_args) $< -o $@

