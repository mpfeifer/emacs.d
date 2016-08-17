default: clean TAGS

clean:
	rm -v TAGS
TAGS:
	etags */*.py $(cygpath.exe -m ~/.emacs.d/templates/template.py)

dos2unix:	
	dos2unix loghost/logwebmon.py
