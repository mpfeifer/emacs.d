default: clean TAGS

clean:
	rm -v TAGS
TAGS:
	etags */*.py $(cygpath.exe -m ~/.emacs.d/templates/template.py)
	
