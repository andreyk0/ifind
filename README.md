ifind
=====

Interactive version of 'find' utility, to quickly open files in VIM.


```
# ifind --help
The ifind program

ifind [OPTIONS]
  Interactive 'find' utility, search file names with regexes.  Search text
  takes multiple regexes, separated by ! which has an effect similar to "..
  |grep -v ..". Ctrl-u toggles case (in)sensitive search. $HOME/.ifind contains
  default config (default exclusion rules).

Common flags:
  -i --indir=ITEM        directory to search recursively
  -o --outfile=ITEM      output file name
  -s --searchre=ITEM     initial value of search regex
  -n --nodefaultfilters  ignore default directory/path filters found in the
                         config
  -c --caseinsensitive   turns on case-insensitive search
  -? --help              Display help message
  -V --version           Print version information
```
