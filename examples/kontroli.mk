%.deps:
	echo $^

# one-liner with tr & awk to remove duplicates while keeping order due to:
# https://catonmat.net/blog/wp-content/uploads/2008/09/awk1line.txt
%.koo:
	$(MAKE) --silent $*.deps | \
	  tr ' ' '\n' | awk '!a[$$0]++' | \
	  grep -v ".deps" | \
	  xargs kontroli
