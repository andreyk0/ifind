
build:
	stack build

clean:
	stack clean

install:
	stack install

tags:
	hasktags --ctags --extendedctag  . ../vty ../vty-ui
