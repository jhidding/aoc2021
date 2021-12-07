.PHONY: site clean watch watch-pandoc watch-browser-sync

pandoc_args += -s -t html5 -f commonmark_x --toc --toc-depth 2
pandoc_args += --template templates/dark.html
pandoc_args += --css dark.css
pandoc_args += --mathjax
pandoc_args += --highlight-style templates/dark.theme
pandoc_args += --lua-filter scripts/hide.lua
pandoc_args += --lua-filter scripts/gnuplot.lua
pandoc_args += --lua-filter scripts/annotate.lua
pandoc_input := README.md $(wildcard lit/day*.md) lit/boilerplate.md
pandoc_output := docs/index.html

static_files := templates/dark.css templates/structure.jpg
static_targets := $(static_files:templates/%=docs/%)
functional_deps := Makefile scripts/gnuplot.lua scripts/annotate.lua templates/dark.html templates/dark.theme

site: $(pandoc_output) $(static_targets)

clean:
	rm -rf docs

$(static_targets): docs/%: templates/%
	@mkdir -p $(@D)
	cp $< $@

docs/index.html: $(pandoc_input) $(functional_deps)
	@mkdir -p $(@D)
	pandoc $(pandoc_args) -o $@ $(pandoc_input)

# Starts a tmux with Entangled, Browser-sync and an Inotify loop for running
# Pandoc.
watch:
	@tmux new-session make --no-print-directory watch-pandoc \; \
		split-window -v make --no-print-directory watch-browser-sync \; \
		split-window -v entangled daemon \; \
		select-layout even-vertical \;

watch-pandoc:
	@while true; do \
		inotifywait -e close_write lit templates scripts Makefile README.md; \
		make site; \
	done

watch-browser-sync:
	browser-sync start -w -s docs

